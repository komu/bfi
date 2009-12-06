(import (rnrs base)
        (rnrs control)
	(rnrs lists)
        (rnrs io simple)
	(bf asm-machine)
	(bf assemble))

(define (display-assembly e)
  (display (optimize (assemble e)))
  (newline))

(define-machine foo m
                (reg.val reg.argl reg.env reg.unenv reg.continue)
  
  (let ([copy-reg (machine-copy m)]
	[load-value (machine-load m)]
	[store-value (machine-store m)])
    (seq (store-value (const 26) (const 65))
	 (load-value (const 26) reg.val)
	 
	 (show-value reg.val)

	 (set-constant reg.val 10)
	 (show-value reg.val)
	 )))

(display-assembly (machine-assemble foo))
