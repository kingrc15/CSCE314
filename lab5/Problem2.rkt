#lang racket

(define (trafficLight color)
  (cond
    ((eq? color "green") (display "It's green\n"))
    ((eq? color "yellow") (display "It's yellow!\n"))
    ((eq? color "red") (display "stop\n"))
    (else (display "wrong color\n"))
    )
  )

(trafficLight "blue")
(trafficLight "green")