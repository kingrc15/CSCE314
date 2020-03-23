(define (mergelist ls1 ls2)
 (cond
  ((and (null? ls1) (null? ls2)) '())
  ((null? ls1) ls2)
  ((null? ls2) ls1)
  ((eq? (car ls1) (car ls2)) (cons (car ls1) (mergelist (cdr ls1) ls2)))
  ((> (car ls1) (car ls2)) (cons (car ls2) (mergelist ls1 (cdr ls2))))
  (else (cons (car ls1) (mergelist (cdr ls1) ls2)))
  ) 
)

(define (main)
  (display (mergelist '(1 5 9 13 72) '( 2 4 7 8 9)))
)

(main)
(exit)

