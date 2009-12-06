#!r6rs
(library (bf asm-primitives)
  (export seq display-current loop inc-current dec-current move-left move-right
	  move-pointer add-current)
  (import (rnrs base)
	  (rnrs records syntactic)
	  (bf utils))

;; Primitives

(define (seq . args) 
  (if (= 1 (length args))
      (car args)
      (cons 'seq args)))

(define (display-current)
  (list 'display))

(define (loop . args)
  (cons 'loop args))

(define (inc-current)
  (list 'inc))

(define (dec-current)
  (list 'dec))

(define (move-left)
  (list 'move-left))

(define (move-right)
  (list 'move-right))

;;

(define (replicate-commands n e)
  (apply seq (replicate n e)))

(define (move-pointer n) 
  (replicate-commands (abs n) (if (positive? n) (move-right) (move-left))))

(define (add-current n)
  (replicate-commands (abs n) (if (positive? n) (inc-current) (dec-current))))


)