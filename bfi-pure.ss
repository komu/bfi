(import (rnrs base)
	(rnrs records syntactic)
	(rnrs io simple))

;; The state is defined as a zipper with three fields
;;  - left: cells to the left of current cell, in reverse order
;;  - current: the current cell
;;  - right: cells to the right of current cell
;;
;; This structure allows for a purely functional implementation that
;; supports all Brainfuck primitives in O(1) time.
(define-record-type state
  (fields left current right))

;; In an initial state there are no cells to the left, since we are
;; at the beginning of the tape. The current cell is zero. We don't
;; need to allocate cells on the right, since they are lazily created
;; as needed (see move-right).
(define (initial-state)
  (make-state '() 0 '()))

;; Moves to the left on the tape; it is an error to go past the left edge.
(define (move-left s)
  (let ([left (state-left s)])
    (if (null? left)
	(error "can't move to left")
	(make-state (cdr left) 
		    (car left) 
		    (cons (state-current s) (state-right s))))))

;; Moves to the right on the tape, allocating more space if necessary.
(define (move-right s)
  (define (normalize p) 
    (if (null? p) (list 0) p)) 

  (let ([right (normalize (state-right s))])
    (make-state (cons (state-current s) (state-left s)) 
		(car right)
		(cdr right))))

;; Returns a new state where the current element has been transformed
;; with given function.
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
  (make-state (state-left s) 
	      (char->integer (read-char))
	      (state-right s)))

;; Given a lexer, parses a Brainfuck program into an abstract syntax tree.
;;
;; The given lexer can be any function which either returns next character
;; or eof-object when called without arguments.
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
	    [(#\[) (let* ([loop (parse-exps)]
			  [rest (parse-exps)])
		     (cons (cons 'loop loop) rest))]
	    [(#\]) '()]
	    [else (parse-exps)]))))
  (parse-exps))

;; Transforms parsed AST into an analyzed representation, which
;; is faster to execute. The result is a function representing
;; the program: the function can be called with the initial state
;; of the machine and will return the final state after stopping.
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
	  (if (zero? (state-current s))
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

;; Creates a lexer from given string.
(define (string->lexer s)
  (list->lexer (string->list s)))

;; Creates a lexer from given list of characters.
(define (list->lexer input)
  (lambda ()
    (if (null? input)
	(eof-object)
	(let ([ch (car input)])
	  (set! input (cdr input))
	  ch))))

;; Parses, analyzes and executes the program provided by given lexer.
(define (run lexer)
  (let* ([exp (parse lexer)]
	 [analyzed (analyze-program exp)])
    (analyzed (initial-state))))

(run read-char)
