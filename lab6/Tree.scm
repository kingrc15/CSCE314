#lang racket

(define (leaf? node)
  (null? (children node)))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (count-leaves tree)
  (if (leaf? tree)
      1
      (count-leaves-in-forest (children tree))
  )
)

(define (count-leaves-in-forest forest)
  (if (null? forest)
      0
      (+ (count-leaves (car forest))
         (count-leaves-in-forest (cdr forest)))
  )
)

(define (make-node datum children)
  (cons datum children))

(define (leaf datum)
  (make-node datum '())
)

(define (cities name-list)
  (map leaf name-list)
)

(define (in-tree? place tree)
  (or (equal? place (datum tree))
      (in-forest? place (children tree))
  )
)

(define (in-forest? place forest)
  (if (null? forest)
      #f
      (or (in-tree? place (car forest))
	    (in-forest? place (cdr forest)))
  )
)

(define (locate city tree)
  (if (equal? city (datum tree))
      (list city)
      (let ((subpath (locate-in-forest city (children tree))))
        (if subpath
            (cons (datum tree) subpath)
            #f))))

(define (locate-in-forest city forest)
  (if (null? forest)
      #f
      (or (locate city (car forest))
	  (locate-in-forest city (cdr forest)))))

(define world-tree2
  (make-node
   'world
   (list (make-node
          'italy
          (cities '(venezia riomaggiore firenze roma)))
         (make-node
          '(united states)
          (list (make-node
                 'california
                 (cities '(berkeley (san francisco) gilroy)))
                (make-node
                 'massachusetts
                 (cities '(cambridge amherst sudbury)))
                (make-node 'ohio (cities '(kent)))))
         (make-node 'zimbabwe (cities '(harare hwange)))
         (make-node 'china
		        (cities '(beijing shanghai guangzhou suzhou)))
         (make-node
          '(great britain)
          (list 
           (make-node 'england (cities '(liverpool)))
           (make-node 'scotland
		            (cities '(edinburgh glasgow (gretna green))))
           (make-node 'wales (cities '(abergavenny)))))
         (make-node
          'australia
          (list
           (make-node 'victoria (cities '(melbourne)))
           (make-node '(new south wales) (cities '(sydney)))
           (make-node 'queensland
		            (cities '(cairns (port douglas))))))
         (make-node 'honduras (cities '(tegucigalpa))))))




; (count-leaves world-tree)
;(display (count-leaves world-tree2))
;(newline)

;> (in-tree? 'abergavenny world-tree)
;(display (in-tree? 'abergavenny world-tree2))
;(newline)
;#T

;> (in-tree? 'abbenay world-tree)
;(display (in-tree? 'abbenay world-tree2))
;(newline)
;#F

;> (in-tree? 'venezia (cadr (children world-tree)))
;(display (in-tree? 'venezia (cadr (children world-tree2))))
;(newline)
;#F


; Problem 1
(display (car (cadddr (caddr (cdddr world-tree2)))))
(newline)

; Problem 2
(define (count-non-leaves tree)
  (if (leaf? tree)
      0
      (count-non-leaves-in-forest (children tree))
  )
)

(define (count-non-leaves-in-forest forest)
  (if (null? forest)
      1
      (+ (count-non-leaves (car forest))
         (count-non-leaves-in-forest (cdr forest)))
  )
)

(display (count-non-leaves world-tree2))
(newline)


; Problem 3
(define (first-child-tree place tree)
  (cond
    ((eq? place (car tree)) (if (= 1 (length tree))
                                '()
                                (car (cadr tree))
                                )
                            )
    ((in-tree? place tree) (first-child-tree place (cdr tree)))
    (else (first-child-tree place (car tree)))
    )
  )

(display (first-child-tree 'china world-tree2))
(newline)
(display (first-child-tree 'liverpool world-tree2))
(newline)
(display (first-child-tree 'italy world-tree2))


; Problem 4
(define (leafDisplay tree)
  (if (leaf? tree)
      tree
      (leafDisplayInForest (children tree))
  )
)

(define (leafDisplayInForest forest)
  (if (null? forest)
      '()
      (append (leafDisplay (car forest)) (leafDisplayInForest (cdr forest)))))

(display (leafDisplay world-tree2))

(newline)
(display (leafDisplay world-tree2))


;Problem 5
(define (replace place1 place2 tree)
  (if (equal? place1 (datum tree))
      (make-node place2 (children tree))
      (make-node (datum tree) (replace-in-forest place1 place2 (children tree)))))
(define (replace-in-forest place1 place2 forest)
  (if (null? forest) '() (cons (replace place1 place2 (car forest))
                               (replace-in-forest place1 place2 (cdr forest)))))

(newline)
(display world-tree2)
(newline)
(display (replace 'gilroy 'tunak world-tree2))