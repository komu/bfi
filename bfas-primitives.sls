(library (bfas-primitives)
  (export seq loop move-pointer add transfer add-constant subtract-constant
	  add-and-zero add-and-zero-relative add-with-scratch loop-while-not-equal)
  (import (rnrs base))

(define (replicate n x)
  (if (zero? n) '() (cons x (replicate (- n 1) x))))

(define (seq . args) 
  (if (= 1 (length args))
      (car args)
      (cons 'seq args)))

(define (loop . args)
  (cons 'loop args))

(define (move-pointer n) 
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
  (seq (move-pointer offset)
       (apply seq items)
       (move-pointer (- offset))))

(define (set-constant offset value)
  (with-offset offset
	       (loop (dec)) ; set to zero
	       (add value)))

(define (add-constant offset value)
  (with-offset offset
	       (add value)))

(define (subtract-constant offset value)
  (add-constant offset (- value)))

(define (map-seq f xs)
  (apply seq (map f xs)))

(define (add-and-zero source targets)
  (let ([targets (if (pair? targets) targets (list targets))])
    (with-offset source
		 (loop (dec)
		       (map-seq (lambda (target)
				  (with-offset (- target source) (inc)))
				targets)))))

(define (add-with-scratch source scratch target)
  (seq (add-and-zero source (list target scratch))
       (add-and-zero scratch source)))

(define (add-and-zero-relative loc offset)
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

)