(import (rnrs base)
        (rnrs control)
	(rnrs lists)
        ;;(rnrs syntax-case)
        ;;(rnrs records syntactic)
        (rnrs io simple)
	(bfas-primitives)
	(utils))

(define (assemble tree)
  (list->string 
   (optimize
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
	    [else         (error "unknown tree -- " tree)]))))))

;; fixes most braindead constructs by removing adjacent +- and <> -elements
(define (optimize e)
  (define redundants (map string->list '("<>" "><" "+-" "-+"))) 

  (if (pair? e)
      (if (member (take 2 e) redundants)
	  (optimize (cddr e))
	  (cons (car e) (optimize (cdr e))))
      e))

(define mem.arg1 1)
(define mem.arg2 2)
(define mem.tmp1 0)
(define mem.tmp2 1)
(define mem.value 2)
(define mem.frame-size 3)

(define (store-primitive)
  (define (find-frame)
    (seq (add-and-zero mem.arg1
		       (+ mem.frame-size mem.tmp1)) ; copy arg1 to tmp1 of first frame
	 (move-pointer mem.frame-size)              ; move to first frame
	 (loop (subtract-constant mem.tmp1 1)   ; while tmp1 is non-zero, subtract 1
	       (add-and-zero-relative mem.tmp1 mem.frame-size)        
	       (add-and-zero-relative mem.tmp2 mem.frame-size)
	       (move-pointer mem.frame-size))))
  
  (define (move-back-to-beginning)
    (loop-while-not-equal mem.tmp1 1
			  (move-pointer (- mem.frame-size))))

  (seq (find-frame)
       (transfer mem.tmp2 mem.value)
       (move-back-to-beginning)))

(define (load-primitive)
  (define (find-frame)
    (seq (add-and-zero mem.arg1
		       (+ mem.frame-size mem.tmp1)) ; copy arg1 to tmp1 of first frame
	 (move-pointer mem.frame-size)
	 (loop (subtract-constant mem.tmp1 1)   ; while tmp1 is non-zero, subtract 1
	       (add-and-zero-relative mem.tmp1 mem.frame-size)
	       (move-pointer mem.frame-size))))

  (define (return-back)
    (loop-while-not-equal mem.tmp1 1
			  (add-and-zero-relative mem.tmp2 (- mem.frame-size))
			  (move-pointer (- mem.frame-size))))

  (seq (find-frame)
       (add-with-scratch mem.value mem.tmp1 mem.tmp2) ; copy value to tmp2 using tmp1
       (return-back)))


(define (test e)
  (display (assemble e))
  (newline))

;; 



(define store-primitive2
  ">[->>+<<]>>[-[->>>+<<<]>[->>>+<<<]>>]>>[-]<[->+<]<-[+<<<-]+")

(define load-primitive2
  ">[->>+<<]>>[-[->>>+<<<]>>>]>>[-<+<+>>]<<[->>+<<]-[+>[-<<<+>>>]<<<<-]+")

(display store-primitive2) (newline)
(test (store-primitive)) (newline)

(display load-primitive2) (newline)
(test (load-primitive)) (newline)
