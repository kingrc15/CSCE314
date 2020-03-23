#lang racket

(define (intersection lst1 lst2)
  (cond
    ((null? lst1) '())
    ((exist? (car lst1) lst2) (cons (car lst1) (intersection (cdr lst1) lst2)))
    ((intersection (cdr lst1) lst2))
    )
  )

(define (exist? X lst)
  (cond
    ((null? lst) #f)
    ((eq? X (car lst)) #t)
    (#t (exist? X (cdr lst)))
    )
  )

(define (main)
  (display (intersection '(1 2 3 4) '(4 -1 2 5)))
  (newline)
  (display (intersection '(5 6 7 8) '(1 2 3)))
  (newline)
  )

(main)