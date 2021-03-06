(define (alternate-first ls)
  (cond
   ((null? ls) '())
   (else (cons (car ls) (alternate-first (if (< (length ls) 2) (cdr ls) (cddr ls)))))
   )
)

(define (main)
  (display (alternate-first '(2 3 4 5 6 9 11)))
  (newline)
  (display (alternate-first '(2 3 4 5 6 9)))
  (newline)
 )

(main)
(exit)
