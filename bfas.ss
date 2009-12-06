(import (rnrs base)
        (rnrs io simple)
	(bf asm-machine)
	(bf assemble))

(define (show-constant-char c) 
  (show-value (const/char c)))

(define (show-constant-string s)
  (map-seq show-constant-char (string->list s)))

(define-machine foo
                (reg.val reg.argl reg.env reg.unenv reg.continue)
		(load store)
  (store (const 26) (const/char #\a))
  (copy reg.val (const 26))
  (store reg.val (const/char #\b))
  (load reg.val reg.val)
  (show-value reg.argl)
  (show-constant-string "Hello, world!\n"))

(let ([asm (optimize (assemble (machine-assemble foo)))])
  (display asm)
  (newline))
