#!r6rs
(library (bs asm-machine)
  (export define-machine machine-assemble machine-copy machine-load machine-store
	  seq transfer set-constant add-constant subtract-constant
	  add-and-zero add-and-zero/relative add-with-scratch loop-while-not-equal
	  const show-value)
  (import (rnrs base)
	  (rnrs records syntactic)
	  (bf asm-primitives)
	  (bf utils))

(define-record-type machine
  (fields initializer copy load store))

(define-record-type constant
  (fields value))

(define const make-constant)

; export op-codes

(define (show-value reg)
  (with-offset reg (display-current)))


(define (with-offset offset . items)
  (seq (move-pointer offset)
       (apply seq items)
       (move-pointer (- offset))))

(define (set-constant offset value)
  (with-offset offset
	       (loop (dec-current)) ; set to zero
	       (add-current value)))

(define (add-constant offset value)
  (with-offset offset (add-current value)))

(define (subtract-constant offset value)
  (add-constant offset (- value)))

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
  (seq (set-constant target 0)
       (add-and-zero source target)))

(define (loop-while-not-equal offset value . body)
  (seq (subtract-constant offset value)   ; subtract to get zero if value is equal
       (loop (add-constant offset value)  ; restore the value
	     (apply seq body)
	     (subtract-constant offset value))
       (add-constant offset value)))      ; restore the value

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
	 (loop (subtract-constant mem.tmp1 1)       ; while tmp1 is non-zero, subtract 1
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
	 (loop (subtract-constant mem.tmp1 1)   ; while tmp1 is non-zero, subtract 1
	       (add-and-zero/relative mem.tmp1 mem.frame-size) ; move tmp1 to next frame
	       (mem.move-to-next-frame))))

  (define (return-back)
    (loop-while-not-equal mem.tmp1 1
			  (add-and-zero/relative mem.tmp2 (- mem.frame-size))
			  (mem.move-to-previous-frame)))

  (seq (find-frame)
       (add-with-scratch mem.value mem.tmp1 mem.tmp2) ; copy value to tmp2 using tmp1
       (return-back)))

(define (make-memory mem.offset reg.scratch)
  (define (copy-reg source target)
    (seq (set-constant target 0)
	 (add-with-scratch source reg.scratch target)))

  (define reg.mem.arg1 (+ mem.offset mem.arg1))
  (define reg.mem.arg2 (+ mem.offset mem.arg2))

  (define (address-offset addr)
    (+ mem.offset (* (+ 1 addr) mem.frame-size) mem.value))

  (define (load-value address reg.target)
    (if (constant? address)
	(copy-reg (address-offset (constant-value address)) reg.target)
	(seq (copy-reg address reg.mem.arg1)
	     (with-offset mem.offset (mem.load-primitive))
	     (copy-reg reg.mem.arg1 reg.target))))

  (define (store-value address value)
    (if (constant? address)
	(if (constant? value)
	    (set-constant (address-offset (constant-value address))
			  (constant-value value))
	    (copy-reg value (address-offset (constant-value address))))
	(seq (copy-reg address reg.mem.arg1)
	     (copy-reg value reg.mem.arg2)
	     (with-offset mem.offset (mem.store-primitive)))))
  
  (make-machine (set-constant mem.offset 1)
		copy-reg
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
       (let ([c (make-counter 0)])
	 (define-machine-internal c m regs body ...)))]))

)