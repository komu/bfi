(import (rnrs base)
        (rnrs control)
        (rnrs syntax-case)
        (rnrs records syntactic)
        (rnrs io simple)
        (rnrs bytevectors)
	(bfas-primitives))

(define (output tree)
  (unless (null? tree)
          (case (car tree)
            [(inc)        (display #\+)]
            [(dec)        (display #\-)]
            [(move-left)  (display #\<)]
            [(move-right) (display #\>)]
            [(display)    (display #\.)]
            [(read)       (display #\,)]
            [(loop)       (display #\[) (for-each output (cdr tree)) (display #\])]
            [(seq)        (for-each output (cdr tree))]
	    [(raw)        (display (cadr tree))]
            [else         (error "unknown tree -- " tree)])))

(define (optimize e)
  e)

(define (test e)
  (output (optimize e))
  (newline))

;; 


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
	 (move mem.frame-size)                  ; move to first frame
	 (loop (subtract-constant mem.tmp1 1)   ; while tmp1 is non-zero, subtract 1
	       (add-and-zero-relative
		mem.tmp1 mem.frame-size)        ; and move its value to next frame
	       (add-and-zero-relative 
		mem.tmp2 mem.frame-size)
	       (move mem.frame-size))))

  (define (move-back-to-beginning)
    (seq (subtract-constant mem.tmp1 1)    ; subtract one from tmp1 to check for sentinel
	 (loop (add-constant mem.tmp1 1)   ; restore tmp1 to zero
	       (move (- mem.frame-size))
	       (subtract-constant mem.tmp1 1)) ; continue loop with previous frame
	 (add-constant mem.tmp1 1)))         ; restore sentinel mark to 1

  (seq (find-frame)
       (transfer mem.tmp2 mem.value)
       (move-back-to-beginning)))

(define (load-primitive)
  (seq))

(define store-primitive2
  ">[->>+<<]>>[-[->>>+<<<]>[->>>+<<<]>>]>>[-]<[->+<]<-[+<<<-]+")

(define load-primitive2
  ">[->>+<<]>>[-[->>>+<<<]>>>]>>[-<+<+>>]<<[->>+<<]-[+>[-<<<+>>>]<<<<-]+")

;(define store-primitive2 (make-store-primitive))
	   ;[loop)))
 


;(display store-primitive2) (newline)
;(test (store-primitive))

(display load-primitive2) (newline)
(test (load-primitive))

;(test `(seq (with-relative-address 4 (add 2) ,store-primitive ,load-primitive)
;            ))
 
