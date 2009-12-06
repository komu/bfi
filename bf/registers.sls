#!r6rs
(library (bf registers)
  (export register-maker make-register register? register-offset
	  define-scratch-register)
  (import (rnrs base)
	  (rnrs records syntactic)
	  (bf utils))

(define-record-type register
  (fields offset))

(define *defined-scratch-registers* 0)

(define (register-maker)
  (let ([counter (make-counter *defined-scratch-registers*)])
    (lambda (name) 
      (make-register (counter)))))

(define (make-scratch-register name)
  (let ([reg (make-register *defined-scratch-registers*)])
    (set! *defined-scratch-registers* (+ 1 *defined-scratch-registers*))
    reg))

(define-syntax define-scratch-register
  (syntax-rules ()
    [(_ name)
     (define name (make-scratch-register (quote name)))]))

)
