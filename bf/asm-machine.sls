#!r6rs
(library (bs asm-machine)
  (export define-machine machine-assemble
	  seq copy copy/reset add subtract show-value
	  const const/char
	  map-seq)
  (import (rnrs base)
	  (rnrs records syntactic)
	  (bf asm-primitives)
	  (bf registers)
	  (bf utils))

(define-record-type machine
  (fields initializer load store))

(define-record-type constant
  (fields value))

(define (const n) (make-constant n))
(define (const/char c) (const (char->codepoint c)))

(define-scratch-register reg.scratch0)
(define-scratch-register reg.scratch1)

; exported op-codes

(define (show-value value)
  (cond [(constant? value)
	 (seq (copy reg.scratch0 value)
	      (display-current)
	      (copy reg.scratch0 (const 0)))]
	[(register? value)
	 (with-register value (display-current))]
	[else 
	 (error "show-value: invalid value -- " value)]))

(define (copy target source)
  (check (register? target) "copy: target is not a register -- " target)
  (cond [(register? source)
	 (seq (copy target (const 0))
	      (add target source))]
	[(constant? source)
	 (with-register target
			(loop (dec-current)) ; set to zero
			(add-current (constant-value source)))]
	[else 
	 (error "copy: invalid source operand -- " + source)]))

;; copies source to target and resets the source to zero
;; todo: optimize this implementation
(define (copy/reset target source)
  (seq (copy target source)
       (copy source (const 0))))

(define (add target value)
  (check (register? target) "add: target is not a register -- " target)
  (cond [(register? value)
	 (primitive-add-with-scratch (register-offset target)
				     (register-offset value)
				     (register-offset reg.scratch0))]
	[(constant? value)
	 (primitive-add (register-offset target) (constant-value value))]
	[else 
	 (error "add: invalid operand -- " + value)]))

(define (subtract reg value)
  (check (register? reg) "subtract: target is not a register --" reg)
  (cond [(register? value)
	 (error "subtract with register is not supported -- " reg)]
	[(constant? value)
	 (add reg (const (- (constant-value value))))]
	[else 
	 (error "subtract: invalid operand -- " + value)]))

;; exported helpers

(define (map-seq f xs)
  (apply seq (map f xs)))

(define (machine-assemble m) 
  (m))

;; internal implementation

(define-syntax with-register
  (syntax-rules ()
    [(_ reg op ...)
     (with-offset (register-offset reg) op ...)]))

(define-syntax with-offset
  (syntax-rules ()
    [(_ offset-exp op ...)
     (let ([offset offset-exp])
       (seq (move-pointer offset)
	    op ...
	    (move-pointer (- offset))))]))

(define (check condition . args)
  (if (not condition) (apply error args)))

;;
;; Primitives operations
;;
;; These operations don't know anything about registers or constants
;; they operate on raw values. They don't use any temporary scratches
;; other than what have been explicitly specified in their argument list.
;;

;; adds the value of source to targets and resets the source to zero
(define (primitive-add/reset source targets)
  (define (increment-target target)
    (with-offset (- target source) (inc-current)))

  (let ([targets (if (pair? targets) targets (list targets))])
    (with-offset source
		 (loop (dec-current)
		       (map-seq increment-target targets)))))

;; adds source to target preserving value of source using scratch
(define (primitive-add-with-scratch target source scratch)
  (check (and (integer? target) (integer? scratch) (integer? source))
	 "add-with-scratch: invalid arguments -- " target scratch source)
  (seq (primitive-add/reset source (list target scratch))
       (primitive-add/reset scratch source)))

(define (primitive-add offset value)
  (check (and (integer? offset) (integer? value))
	 "primitive-add: invalid arguments -- " offset value)

  (with-offset offset (add-current value)))

(define (primitive-subtract offset value)
  (primitive-add offset (- value)))

(define (primitive-loop-while-not-equal offset value . body)
  (seq (primitive-subtract offset value)   ; subtract to get zero if value is equal
       (loop (primitive-add offset value)  ; restore the value
	     (apply seq body)
	     (primitive-subtract offset value))
       (primitive-add offset value)))      ; restore the value


;;
;; Random access memory abstraction
;;

(define mem.frame-size 3)
(define mem.arg1 1)
(define mem.arg2 2)
(define mem.tmp1 0)
(define mem.tmp2 1)
(define mem.value 2)
(define mem.next.tmp1 (+ mem.tmp1 mem.frame-size))
(define mem.next.tmp2 (+ mem.tmp2 mem.frame-size))
(define mem.previous.tmp1 (+ mem.tmp1 (- mem.frame-size)))
(define mem.previous.tmp2 (+ mem.tmp2 (- mem.frame-size)))

(define (mem.move-to-next-frame)     (move-pointer mem.frame-size))
(define (mem.move-to-previous-frame) (move-pointer (- mem.frame-size)))

;; given address in arg1 and optional parameter in arg2, moves to the frame
;; given by arg1 and moving value to arg2 to tmp2 of given frame.
(define (mem.find-frame)
  (seq (primitive-add/reset mem.arg1 mem.next.tmp1)
       (primitive-add/reset mem.arg2 mem.next.tmp2)
       (mem.move-to-next-frame)
       (loop (primitive-subtract mem.tmp1 1) ; subtract address until frame is found
	     (primitive-add/reset mem.tmp1 mem.next.tmp1)
	     (primitive-add/reset mem.tmp2 mem.next.tmp2)
	     (mem.move-to-next-frame))))

(define (mem.store-primitive)
  (define (return-back)
    (primitive-loop-while-not-equal mem.tmp1 1 
      (mem.move-to-previous-frame)))
  
  (seq (mem.find-frame)
       (copy (make-register mem.value) (const 0))
       (primitive-add/reset mem.value mem.tmp2)
       (return-back)))

(define (mem.load-primitive)
  (define (return-back)
    (primitive-loop-while-not-equal mem.tmp1 1
      (primitive-add/reset mem.tmp2 mem.previous.tmp2)
      (mem.move-to-previous-frame)))

  (seq (mem.find-frame)
       (primitive-add-with-scratch mem.tmp2 mem.value mem.tmp1)
       (return-back)))

(define (build-machine mem.offset)
  (define reg.mem.arg1 (make-register (+ mem.offset mem.arg1)))
  (define reg.mem.arg2 (make-register (+ mem.offset mem.arg2)))

  (define (address-register addr)
    (make-register (+ mem.offset (* (+ 1 addr) mem.frame-size) mem.value)))

  (define (load-value target address)
    (check (register? target) "load-value: target is not a register -- " target)
    (cond [(constant? address)
	   (copy target (address-register (constant-value address)))]
	  [(register? address)
	   (seq (copy reg.mem.arg1 address)
		(with-offset mem.offset (mem.load-primitive))
		(copy/reset target reg.mem.arg1))]
	  [else
	   (error "load-value: invalid address -- " address)]))

  (define (store-value address value)
    (cond [(constant? address)
	   (copy (address-register (constant-value address)) value)]
	  [(register? address)
	   (seq (copy reg.mem.arg1 address)
		(copy reg.mem.arg2 value)
		(with-offset mem.offset (mem.store-primitive)))]
	  [else
	   (error "store-value: invalid address -- " address)]))
  
  (make-machine (copy (make-register mem.offset) (const 1))
		load-value
		store-value))

(define-syntax define-machine
  (syntax-rules ()
    [(_ name (reg ...) (load store) body ...)
     (define (name)
       (let* ([make-reg (register-maker)]
	      [reg (make-reg (quote reg))] ...
	      [m (build-machine (register-offset (make-reg 'memory)))]
	      [load (machine-load m)]
	      [store (machine-store m)])
	 (seq (machine-initializer m) body ...)))]))

)