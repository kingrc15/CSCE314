(define (is-negative num)
  (if (< num 0) #t #f)
)

(define (main)
  (display (is-negative -100))
  (newline)
  (display (is-negative 0))
  (newline)
  (display (is-negative 100))
)

(main)
(exit)
