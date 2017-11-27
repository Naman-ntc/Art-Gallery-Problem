#lang racket

(provide (all-defined-out))

(define edge%
  (class object%
    (super-new)                                                         ;;;;;;;;;;;;;;;;counterclockwise
    (init-field first-vertex)
    (init-field second-vertex)
    (init-field index-first-vertex-in-vertex-vector)
    (init-field index-second-vertex-in-vertex-vector)
    (field [x-first (get-field x-coor first-vertex)])
    (field [y-first (get-field y-coor first-vertex)])
    (field [x-second (get-field x-coor second-vertex)])
    (field [y-second (get-field y-coor second-vertex)])
    (field [x2-x1 (- x-second x-first)])
    (field (y2-y1 (- y-second y-first)))
    
    (define/public (distance-from-edge-along-x-direction a-vertex)
      (let* [(alpha (get-field x-coor a-vertex))
             (beta (get-field y-coor a-vertex))
             (inverse-slope (/ x2-x1 y2-y1))
             (beta-y1 (- beta y-first))]
        (if (> (+ alpha (- x-first) (- (* beta-y1 inverse-slope))) 0)
               (+ alpha (- x-first) (- (* beta-y1 inverse-slope)))
               (expt 10 6))))))
               
(define orthogonal-edge%
  (class edge%
    (super-new)
    (inherit-field first-vertex second-vertex index-first-vertex-in-vertex-vector index-second-vertex-in-vertex-vector x-first y-first x-second y-second y2-y1 x2-x1)

    (define code-for-type 
      (cond [(= y2-y1 0)
             (if (> x2-x1 0)
                  (cond [(let* [(first-vertex (get-field angle-at-vertex first-vertex))
                                (second-vertex (get-field angle-at-vertex second-vertex))]
                                (and (< first-vertex 360) (< second-vertex 360) (> first-vertex 180) (> second-vertex 180))) 1] ;;;bottom peak
                        [#t 3])  ;;;bottom edge-non-slanted 
                  (cond [(let* [(first-vertex (get-field angle-at-vertex first-vertex))
                                (second-vertex (get-field angle-at-vertex second-vertex))]
                                (and (< first-vertex 360) (< second-vertex 360) (> first-vertex 180) (> second-vertex 180))) 2]   ;;;top peak
                        [#t 4]))] ;;;top edge-non-slanted
            [(= x2-x1 0) 0] ;;;vertical edges
            [(> x2-x1 0) 3] ;;;slanted bottom edge or bottom segment as on pg. 55
            [(< x2-x1 0) 4])) ;;;slanted top edge or top segment as on pg. 55

    (define (set-code-for-type-manually code)
      (set! code-for-type code))
    

    (define/public (edge-type) 
      (cond [(= code-for-type 0) "Vertical Edge"]
            [(= code-for-type 1) "Bottom Peak"]
            [(= code-for-type 2) "Top Peak"]
            [(= code-for-type 3) "Bottom Edge"]
            [(= code-for-type 4) "Top Edge"]))

    (field [chain #f])
    ))
