(import (rnrs base)
        (rnrs control)
	(rnrs lists)
        (rnrs io simple)
	(bf asm-machine)
	(bf assemble))

(define (display-assembly e)
  (display (optimize (assemble e)))
  (newline))

(define (show-constant-string s)
  (define (show-char c) (show-value (const/char c)))
  (map-seq show-char (string->list s)))

(define-machine foo m
                (reg.val reg.argl reg.env reg.unenv reg.continue)
  
  (let ([load (machine-load m)]
	[store (machine-store m)])
    (seq (store (const 26) (const/char #\a))
	 (copy reg.val (const 26))
	 (store reg.val (const/char #\b))
	 (load reg.val reg.val)
	 (show-value reg.val)
	 (show-constant-string "Hello, world!\n")
	 (show-value reg.val))
  ))

(display-assembly (machine-assemble foo))
