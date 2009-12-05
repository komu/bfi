(library (bfas-primitives)
  (export seq loop move add transfer add-constant subtract-constant
	  add-and-zero add-and-zero-relative)
  (import (rnrs base))

(define (replicate n x)
  (if (zero? n) '() (cons x (replicate (- n 1) x))))

(define (seq . args) 
  (if (= 1 (length args))
      (car args)
      (cons 'seq args)))

(define (loop . args)
  (cons 'loop args))

(define (move n) 
  (cons 'seq (replicate (abs n) (if (positive? n) (move-right) (move-left)))))

(define (add n)
  (cons 'seq (replicate (abs n) (if (positive? n) (inc) (dec)))))

(define (inc)
  (list 'inc))

(define (dec)
  (list 'dec))

(define (move-left)
  (list 'move-left))

(define (move-right)
  (list 'move-right))

(define (with-offset offset . items)
  (seq (move offset)
       (apply seq items)
       (move (- offset))))

(define (set-constant offset value)
  (with-offset offset
	       (loop (dec)) ; set to zero
	       (add value)))

(define (add-constant offset value)
  (with-offset offset
	       (add value)))

(define (subtract-constant offset value)
  (add-constant offset (- value)))

(define (add-and-zero from to) 
  (with-offset from
	       (loop (dec)
		     (with-offset (- to from)
				  (inc)))))

(define (add-and-zero-relative loc offset)
  (add-and-zero loc (+ loc offset)))

(define (transfer source target)
  (seq (set-constant target 0)
       (add-and-zero source target)))
)