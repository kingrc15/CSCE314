#lang racket

(define (cal f ls)
  (cond
    ((null? ls) '())
    (else (cons (f (car ls)) (cal f (cdr ls))))
    )
  )

(define (main)
  (display (cal sqrt '(1 2 3 4 5)))
  )

(main)