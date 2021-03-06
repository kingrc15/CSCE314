(define (exists val ls)
  (cond
   ((null? ls) #f)
   ((eq? (car ls) val) #t)
   (else (exists val (cdr ls)))
  )
)

(define (intersection ls1 ls2)
  (cond
   ((null? ls1) '())
   ((exists (car ls1) ls2) (cons (car ls1) (intersection (cdr ls1) ls2)))
   (else (intersection (cdr ls1) ls2))
   )
)

(define (main)
  (display (intersection '(1 2 3 4) '(4 -1 2 5)))
  (newline)
  (display (intersection '(5 6 7 8) '(1 2 3)))
)

(main)
(exit)
