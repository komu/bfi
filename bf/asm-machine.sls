#!r6rs
(library (bs asm-machine)
  (export define-machine machine-assemble copy machine-load machine-store
	  seq copy add subtract show-value
	  const const/char
	  map-seq)
  (import (rnrs base)
	  (rnrs records syntactic)
	  (bf asm-primitives)
	  (bf utils))

(define-record-type machine
  (fields initializer load store))

(define-record-type constant
  (fields value))

(define const make-constant)
(define (const/char c) (const (char->codepoint c)))

(define (make-register n) n)
(define (register? n) (integer? n))
(define (register-offset n) n)

(define reg.scratch (make-register 0))

; exported op-codes

(define (show-value value)
  (with-value value (display-current)))

;; helpers

;; performs selected ops with given value which can either be a constant or register
(define (with-value value . ops)
  (cond [(constant? value)
	 (with-constant (constant-value value) (apply seq ops))]
	[(register? value)
	 (with-register value (apply seq ops))]
	[else 
	 (error "invalid operand -- " value)]))

(define (with-constant constant . items)
  (seq (copy reg.scratch (const constant))
       (apply seq items)
       (copy reg.scratch (const 0))))

(define (with-register reg . items)
  (with-offset (register-offset reg)
	       (apply seq items)))

(define (with-offset offset . items)
  (seq (move-pointer offset)
       (apply seq items)
       (move-pointer (- offset))))

(define (copy target source)
  (cond [(register? source)
	 (seq (copy target (const 0))
	      (add-with-scratch source reg.scratch target))]
	[(constant? source)
	 (with-register target
			(loop (dec-current)) ; set to zero
			(add-current (constant-value source)))]
	[else 
	 (error "copy: invalid source operand -- " + source)]))

(define (add reg value)
  (cond [(register? value)
	 (error "add with register is not supported -- " reg)]
	[(constant? value)
	 (with-register reg (add-current (constant-value value)))]
	[else 
	 (error "add: invalid operand -- " + value)]))

(define (subtract reg value)
  (cond [(register? value)
	 (error "subtract with register is not supported -- " reg)]
	[(constant? value)
	 (add reg (const (- (constant-value value))))]
	[else 
	 (error "subtract: invalid operand -- " + value)]))

(define (map-seq f xs)
  (apply seq (map f xs)))

(define (add-and-zero source targets)
  (define (increment-target target)
    (with-offset (- target source) (inc-current)))

  (let ([targets (if (pair? targets) targets (list targets))])
    (with-offset source
		 (loop (dec-current)
		       (map-seq increment-target targets)))))

(define (add-with-scratch source scratch target)
  (seq (add-and-zero source (list target scratch))
       (add-and-zero scratch source)))

(define (add-and-zero/relative loc offset)
  (add-and-zero loc (+ loc offset)))

(define (transfer source target)
  (seq (copy target (const 0))
       (add-and-zero source target)))

(define (loop-while-not-equal offset value . body)
  (seq (subtract offset (const value))   ; subtract to get zero if value is equal
       (loop (add offset (const value))  ; restore the value
	     (apply seq body)
	     (subtract offset (const value)))
       (add offset (const value))))      ; restore the value

; implementation

(define (machine-assemble m) (m))

(define mem.arg1 1)
(define mem.arg2 2)
(define mem.tmp1 0)
(define mem.tmp2 1)
(define mem.value 2)
(define mem.frame-size 3)

(define (mem.move-to-next-frame)
  (move-pointer mem.frame-size))

(define (mem.move-to-previous-frame)
  (move-pointer (- mem.frame-size)))

(define (mem.store-primitive)
  (define (find-frame)
    (seq (add-and-zero mem.arg1
		       (+ mem.frame-size mem.tmp1)) ; copy arg1 to tmp1 of first frame
	 (move-pointer mem.frame-size)              ; move to first frame
	 (loop (subtract mem.tmp1 (const 1))       ; while tmp1 is non-zero, subtract 1
	       (add-and-zero/relative mem.tmp1 mem.frame-size)        
	       (add-and-zero/relative mem.tmp2 mem.frame-size)
	       (mem.move-to-next-frame))))
  
  (define (move-back-to-beginning)
    (loop-while-not-equal mem.tmp1 1 (mem.move-to-previous-frame)))
  
  (seq (find-frame)
       (transfer mem.tmp2 mem.value)
       (move-back-to-beginning)))

(define (mem.load-primitive)
  (define (find-frame)
    (seq (add-and-zero mem.arg1
		       (+ mem.frame-size mem.tmp1)) ; copy arg1 to tmp1 of first frame
	 (move-pointer mem.frame-size)
	 (loop (subtract mem.tmp1 (const 1))   ; while tmp1 is non-zero, subtract 1
	       (add-and-zero/relative mem.tmp1 mem.frame-size) ; move tmp1 to next frame
	       (mem.move-to-next-frame))))

  (define (return-back)
    (loop-while-not-equal mem.tmp1 1
			  (add-and-zero/relative mem.tmp2 (- mem.frame-size))
			  (mem.move-to-previous-frame)))

  (seq (find-frame)
       (add-with-scratch mem.value mem.tmp1 mem.tmp2) ; copy value to tmp2 using tmp1
       (return-back)))

(define (make-memory mem.offset unused)

  (define reg.mem.arg1 (+ mem.offset mem.arg1))
  (define reg.mem.arg2 (+ mem.offset mem.arg2))

  (define (address-offset addr)
    (+ mem.offset (* (+ 1 addr) mem.frame-size) mem.value))

  (define (load-value address reg.target)
    (cond [(constant? address)
	   (copy reg.target (address-offset (constant-value address)))]
	  [(register? address)
	   (seq (copy reg.mem.arg1 address)
		(with-offset mem.offset (mem.load-primitive))
		(copy reg.target reg.mem.arg1))]
	  [else
	   (error "load-value: invalid address -- " address)]))

  (define (store-value address value)
    (cond [(constant? address)
	   (copy (address-offset (constant-value address)) value)]
	  [(register? address)
	   (seq (copy reg.mem.arg1 address)
		(copy reg.mem.arg2 value)
		(with-offset mem.offset (mem.store-primitive)))]
	  [else
	   (error "store-value: invalid address -- " address)]))
  
  (make-machine (copy mem.offset (const 1))
		load-value
		store-value))

(define-syntax define-machine-internal
  (syntax-rules ()
    [(define-machine-internal c m () body ...)
     (let ([m (make-memory (c) (c))]) 
       (seq (machine-initializer m) body ...))]
    [(define-machine-internal c m (r) body ...)
     (let ([r (c)]) (define-machine-internal c m () body ...))]
    [(define-machine-internal c m (r . rs) body ...) 
     (let ([r (c)]) (define-machine-internal c m rs body ...))]))

(define-syntax define-machine
  (syntax-rules ()
    [(_ name m regs body ...) 
     (define (name)
       (let ([c (make-counter 1)])
	 (define-machine-internal c m regs body ...)))]))

)