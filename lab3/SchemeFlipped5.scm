(define (func f1 f2 x y)
  (if (and (eq? x 1) (eq? y 1)) (+ (f1 x) (f2 y)) 
      (if (or (eq? x "k") (eq? y "k")) "Keishla"
	  (if (> x y) (* (f1 x) (f2 y)) (* x y))
	  )
      )
  )

(define (main)
  (display (func (lambda (x) (+ x 1)) (lambda (y) (+ y 2)) 4 1))
)

(main)
(exit)

