#!r6rs
(library (bf utils)
  (export flatmap take starts-with? starts-with-any? replicate make-counter
	  char->codepoint)
  (import (rnrs base)
	  (rnrs bytevectors))

(define (replicate n x)
  (if (zero? n)
      '()
      (cons x (replicate (- n 1) x))))

(define (flatmap f l)
  (apply append (map f l)))

(define (starts-with? prefix list)
  (or (null? prefix) 
      (and (pair? list)
	   (equal? (car prefix) (car list))
	   (starts-with? (cdr prefix) (cdr list)))))

(define (any? f list)
  (if (null? list)
      #f
      (or (f (car list))
	  (any? f (cdr list)))))

(define (all? f list)
  (or (null? list)
      (and (f (car list))
	   (all? f (cdr list)))))

(define (starts-with-any? prefixes list)
  (any? (lambda (prefix) (starts-with? prefix list)) prefixes))

(define (take n list)
  (if (or (zero? n) (null? list))
      '()
      (cons (car list) (take (- n 1) (cdr list)))))


(define (make-counter count)
  (lambda ()
    (let ([value count])
      (set! count (+ count 1))
      value)))

(define (char->codepoint c)
  (bytevector-u8-ref (string->utf8 (make-string 1 c)) 0))

)