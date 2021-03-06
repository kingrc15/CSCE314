(define (sphereVol d)
  (/ (* 3.14 (* d (* d d))) 6)
)

(define (rectVol h w l)
  (* h (* w l))
)

(define (isContained d h w l)
  (if (<= (sphereVol d) (rectVol h w l)) #t #f)
)


(define (main)
  (display (isContained 19.5 10 8 45))
  (newline)
  (display (isContained 15.5 10 8 48))
  (newline)
  (display (isContained 16 10 8 53))
)

(main)
(exit)
