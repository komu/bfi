#!r6rs
(library (bf assemble)
  (export assemble optimize)
  (import (rnrs base)
          (bf utils))

;; assembles a tree of operations into a string representing Brainfuck program
(define (assemble tree)
  (list->string
   (let recur ([tree tree])
     (if (null? tree)
	 '()
	 (case (car tree)
	   [(inc)        (list #\+)]
	   [(dec)        (list #\-)]
	   [(move-left)  (list #\<)]
	   [(move-right) (list #\>)]
	   [(display)    (list #\.)]
	   [(read)       (list #\,)]
	   [(loop)       (append (list #\[) 
				 (flatmap recur (cdr tree))
				 (list #\]))]
	   [(seq)        (flatmap recur (cdr tree))]
	   [else         (error "invalid tree -- " tree)])))))

;; takes a string representing a Brainfuck program and optimizes it
(define (optimize e)
  (define (optimize-default e)
    (if (null? e)
	'()
	(case (car e)
	  [(#\> #\<) (optimize-sequence #\> #\< e)]
	  [(#\+ #\-) (optimize-sequence #\+ #\- e)]
	  [else      (cons (car e) (optimize-default (cdr e)))])))
  
  (define (optimize-sequence inc dec e)
    (define (generate n)
      (replicate (abs n) (if (positive? n) inc dec)))

    (let loop ([n 0] [e e])
      (if (null? e)
	  (generate n)
	  (cond [(equal? dec (car e)) (loop (- n 1) (cdr e))]
		[(equal? inc (car e)) (loop (+ n 1) (cdr e))]
		[else                 (append (generate n)
					      (optimize-default e))]))))
  
  (list->string (optimize-default (string->list e))))

)