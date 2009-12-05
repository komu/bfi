(library (utils)
  (export flatmap take starts-with? starts-with-any?)
  (import (rnrs base))

(define (flatmap f l)
  (apply append (map f l)))

(define (starts-with? prefix list)
  (or (null? prefix) 
      (and (pair? list)
	   (equal? (car prefix) (car list))
	   (starts-with? (cdr prefix) (cdr list)))))

(define (starts-with-any? prefixes list)
  (if (null? prefixes)
      #f
      (or (starts-with? (car prefixes) list)
	  (starts-with-any? (cdr prefixes) list))))

(define (take n list)
  (if (or (zero? n) (null? list))
      '()
      (cons (car list) (take (- n 1) (cdr list)))))

)