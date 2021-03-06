(define (CurvedGrade grade f)
  (f grade)
)

(define (f1 grade)
  (* 1.2 grade)
)

(define (f2 grade)
  (+ 5 grade)
)

(define (main)
  (display "Function 1: multiply the grade by 1.2")
  (newline)
  (display "Grade 1: ")
  (display (CurvedGrade 80 f1))
  (newline)
  (display "Function 2: add five to the grade")
  (newline)
  (display "Grade 2: ")
  (display (CurvedGrade 90 f2))
)

(main)
(exit)
