(define (fact n)
  (if (< n 2) 
      n 
      (+ (fact (- n 1)) (fact (- n 2)))
      )
)

(trace fact)

(fact 5)
(fact 6)
(fact 7)
(fact 8)
(fact 9)
(fact 10)
