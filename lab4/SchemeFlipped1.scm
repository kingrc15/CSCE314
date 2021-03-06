(define (listmaker size)
  (cond
   ((eq? size 0) '())
   (else (cons (- size 1) (listmaker (- size 1))))
   )
)

(define (main)
  (display (listmaker 5))
  (newline)
  (display (listmaker 0))
)

(main)
(exit)
