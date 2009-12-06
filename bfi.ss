(import (rnrs base)
	(rnrs control)
	(rnrs syntax-case)
	(rnrs records syntactic)
	(rnrs io simple))

(define-record-type state
  (fields mem (mutable pointer)))

(define initial-state
  (case-lambda 
   [()     (initial-state 30000)]
   [(size) (make-state (make-vector size 0) 0)]))

(define (state-current-value s)
  (vector-ref (state-mem s) (state-pointer s)))

(define (make-move n)
  (lambda (s)
    (state-pointer-set! s (+ n (state-pointer s)))))

(define (make-add n)
  (lambda (s)
    (let ([v (state-mem s)]
	  [i (state-pointer s)])
      (vector-set! v i (+ n (vector-ref v i))))))

(define (make-set n)
  (lambda (s)
    (vector-set! (state-mem s) (state-pointer s) n)))

(define nop
  (lambda (s) 'nop))

(define (display-current s)
  (display (integer->char (state-current-value s))))

(define (read-into-current! state)
  (error "read is unsupported"))

;;(define (char->number c)
;;  (bytevector-ref (string->utf8 (make-string 1 c)) 0))


(define (parse lexer)
  (define (parse-exps)
    (let ([ch (lexer)])
      (if (eof-object? ch)
	  '()
	  (case ch
	    [(#\+) (cons '(add 1)   (parse-exps))]
	    [(#\-) (cons '(add -1)  (parse-exps))]
	    [(#\>) (cons '(move 1)  (parse-exps))]
	    [(#\<) (cons '(move -1) (parse-exps))]
	    [(#\.) (cons '(display) (parse-exps))]
	    [(#\,) (cons '(read)    (parse-exps))]
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
		(lambda (s) (first s) (rest s)))))))
    
  (define (analyze-loop p)
    (let ([body (analyze-seq p)])
      (lambda (s)
	(let loop ()
	  (unless (= 0 (state-current-value s))
		  (body s) 
		  (loop))))))
  
  ;; analyzes single expression
  (define (analyze-exp p)
    (case (car p)
      [(loop)    (analyze-loop (cdr p))]
      [(add)     (make-add (cadr p))]
      [(move)    (make-move (cadr p))]
      [(set)     (make-set (cadr p))]
      [(display) display-current]
      [(read)    read-into-current!]
      [else      (error "invalid form -- " p)]))

  (analyze-seq e))

(define (tagged-value tag p)
  (and (pair? p) (eq? tag (car p)) (cadr p)))

(define (optimize program)
  (define (merge-adjacent tag <+> p)
    (let recur ([p p])
      (define (merge-deeper p) 
	(cons (recur (car p)) 
	      (recur (cdr p))))

      (if (pair? p)
	  (if (pair? (cdr p))
	      (let ([v1 (tagged-value tag (car p))]
		    [v2 (tagged-value tag (cadr p))])
		(if (and v1 v2)
		    (recur (cons (list tag (<+> v1 v2)) (cddr p)))
		    (merge-deeper p)))
	      (merge-deeper p))
	  p)))

  (define (optimize-sets p)
    (cond [(equal? p '(loop (add -1))) 
	   '(set 0)]
	  [(pair? p) 
	   (cons (optimize-sets (car p))
		 (optimize-sets (cdr p)))]
	  [else p]))

  ;; todo: optimizing sets could open up new opportunities for
  ;; merging adjacent adds
  (optimize-sets (merge-adjacent 'move + (merge-adjacent 'add + program))))

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
	 [optimized (optimize exp)]
	 [analyzed (analyze-program optimized)])
    (analyzed (initial-state))))

(run read-char)
