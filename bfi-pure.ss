(import (rnrs base)
	(rnrs records syntactic)
	(rnrs io simple))

(define-record-type state
  (fields left current right))

(define (initial-state)
  (make-state '() 0 '()))

; moves to the left on the tape; it is an error to go past the left edge
(define (move-left s)
  (let ([left (state-left s)])
    (if (null? left)
	(error "can't move to left")
	(make-state (cdr left) 
		    (car left) 
		    (cons (state-current s) (state-right s))))))

; moves to the right on the tape, allocating more space if necessary
(define (move-right s)
  (define (normalize p) 
    (if (null? p) (list 0) p)) 

  (let ([right (normalize (state-right s))])
    (make-state (cons (state-current s) (state-left s)) 
		(car right)
		(cdr right))))

(define (modify-current f s)
  (make-state (state-left s) (f (state-current s)) (state-right s)))

(define (increase s)
  (modify-current (lambda (v) (+ v 1)) s))

(define (decrease s)
  (modify-current (lambda (v) (- v 1)) s))

(define (nop s) s)

(define (display-current s)
  (display (integer->char (state-current s)))
  s)

(define (read-into-current s)
  (let ([value (char->integer (read-char))])
    (make-state (state-left s) value (state-right s))))

(define (parse lexer)
  (define (parse-exps)
    (let ([ch (lexer)])
      (if (eof-object? ch)
	  '()
	  (case ch
	    [(#\+) (cons '(increase)   (parse-exps))]
	    [(#\-) (cons '(decrease)   (parse-exps))]
	    [(#\>) (cons '(move-right) (parse-exps))]
	    [(#\<) (cons '(move-left ) (parse-exps))]
	    [(#\.) (cons '(display)    (parse-exps))]
	    [(#\,) (cons '(read)       (parse-exps))]
	    [(#\]) '()]
	    [(#\[) (let* ([loop (parse-exps)]
			  [rest (parse-exps)])
		     (cons (cons 'loop loop) rest))]
	    [else (parse-exps)]))))
  (parse-exps))

(define (analyze-program e)
  
  ;; analyze a list of expressions and produce a single
  ;; expression which evaluates all sub-expressions in sequence
  (define (analyze-seq p)
    (if (null? p) 
	nop
	(let ([first (analyze-exp (car p))])
	  (if (null? (cdr p))
	      first
	      (let ([rest (analyze-seq (cdr p))])
		(lambda (s) (rest (first s))))))))
    
  (define (analyze-loop p)
    (let ([body (analyze-seq p)])
      (lambda (s)
	(let loop ([s s])
	  (if (= 0 (state-current s))
	      s
	      (loop (body s)))))))
  
  ;; analyzes single expression
  (define (analyze-exp p)
    (case (car p)
      [(loop)       (analyze-loop (cdr p))]
      [(increase)   increase]
      [(decrease)   decrease]
      [(move-left)  move-left]
      [(move-right) move-right]
      [(display)    display-current]
      [(read)       read-into-current]
      [else         (error "invalid form -- " p)]))

  (analyze-seq e))

(define (string->lexer str)
  (list->lexer (string->list str)))

(define (list->lexer input)
  (lambda ()
    (if (null? input)
	(eof-object)
	(let ([ch (car input)])
	  (set! input (cdr input))
	  ch))))

(define (run lexer)
  (let* ([exp (parse lexer)]
	 [analyzed (analyze-program exp)])
    (analyzed (initial-state))))

(run read-char)
