(import (rnrs base)
        (rnrs control)
	(rnrs lists)
        (rnrs io simple)
	(bf asm-primitives)
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

(define mem.arg1 1)
(define mem.arg2 2)
(define mem.tmp1 0)
(define mem.tmp2 1)
(define mem.value 2)
(define mem.frame-size 3)

(define (mem.move-to-next-frame)
  (move-pointer mem.frame-size))

(define (mem.move-to-previous-frame)
  (move-pointer (- mem.frame-size)))

(define (mem.store-primitive)
  (define (find-frame)
    (seq (add-and-zero mem.arg1
		       (+ mem.frame-size mem.tmp1)) ; copy arg1 to tmp1 of first frame
	 (move-pointer mem.frame-size)              ; move to first frame
	 (loop (subtract-constant mem.tmp1 1)       ; while tmp1 is non-zero, subtract 1
	       (add-and-zero/relative mem.tmp1 mem.frame-size)        
	       (add-and-zero/relative mem.tmp2 mem.frame-size)
	       (mem.move-to-next-frame))))
  
  (define (move-back-to-beginning)
    (loop-while-not-equal mem.tmp1 1 (mem.move-to-previous-frame)))
  
  (seq (find-frame)
       (transfer mem.tmp2 mem.value)
       (move-back-to-beginning)))

(define (mem.load-primitive)
  (define (find-frame)
    (seq (add-and-zero mem.arg1
		       (+ mem.frame-size mem.tmp1)) ; copy arg1 to tmp1 of first frame
	 (move-pointer mem.frame-size)
	 (loop (subtract-constant mem.tmp1 1)   ; while tmp1 is non-zero, subtract 1
	       (add-and-zero/relative mem.tmp1 mem.frame-size) ; move tmp1 to next frame
	       (mem.move-to-next-frame))))

  (define (return-back)
    (loop-while-not-equal mem.tmp1 1
			  (add-and-zero/relative mem.tmp2 (- mem.frame-size))
			  (mem.move-to-previous-frame)))

  (seq (find-frame)
       (add-with-scratch mem.value mem.tmp1 mem.tmp2) ; copy value to tmp2 using tmp1
       (return-back)))

;; copies source to target(s) using scratch-register

(define (show-value reg)
  (with-offset reg (display-current)))

(define (display-assembly e)
  (display (optimize (assemble e)))
  (newline))

(define (make-memory offset)
  (lambda (op)
    (case op
      [(arg1)        (+ offset mem.arg1)]
      [(arg2)        (+ offset mem.arg2)]
      [(initializer) (set-constant offset 1)]
      [(offset)      offset]
      [else (error "invalid op -- " op)])))
      

(define-syntax define-machine-internal
  (syntax-rules ()
    [(define-machine-internal c m () a ...)
     (let ([m (make-memory (c))]) a ...)]
    [(define-machine-internal c m (r) a ...)
     (let ([r (c)]) (define-machine-internal c m () a ...))]
    [(define-machine-internal c m (r . rs) a ...) 
     (let ([r (c)]) (define-machine-internal c m rs a ...))]))

(define-syntax define-machine
  (syntax-rules ()
    [(_ n m x a ...) 
     (define (n) (let ([c (make-counter 0)])
		   (define-machine-internal c m x a ...)))]))

(define-machine foo mem (reg.scratch reg.val reg.argl
				     reg.env reg.unenv
				     reg.continue)
  
  ;; register for copying stuff. always zero except when primitive operation uses it

  (define mem.offset (mem 'offset))
  (define reg.mem.arg1 (mem 'arg1))
  (define reg.mem.arg2 (mem 'arg2))

  (define (copy-reg source target)
    (seq (set-constant target 0)
	 (add-with-scratch source reg.scratch target)))
  
  (define (load-value address reg.target)
    (if (constant? address)
	(copy-reg (mem.address-value-offset (constant-value address)) reg.target)
	(seq (copy-reg address reg.mem.arg1)
	     (with-offset mem.offset (mem.load-primitive))
	     (copy-reg reg.mem.arg1 reg.target))))

  (define (store-value address value)
    (if (constant? address)
	(if (constant? value)
	    (set-constant (mem.address-value-offset (constant-value address))
			  (constant-value value))
	    (copy-reg value (mem.address-value-offset (constant-value address))))
	(seq (copy-reg address reg.mem.arg1)
	     (copy-reg value reg.mem.arg2)
	     (with-offset mem.offset (mem.store-primitive)))))

  (define (mem.address-value-offset addr)
    (+ mem.offset (* (+ 1 addr) mem.frame-size) mem.value))

  (seq (mem 'initializer)
       (store-value (const 26) (const 65))
       (load-value (const 26) reg.val)
      
       (show-value reg.val)

       (set-constant reg.val 10)
       (show-value reg.val)
      ))

(display-assembly (foo))
