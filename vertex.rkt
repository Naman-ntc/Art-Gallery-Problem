#lang racket

(provide (all-defined-out))

(define vertex%
  (class object%
    (super-new)
    (init-field x-coor)
    (init-field y-coor)
    
    (define/public (vertex-ycoor) y-coor)
    (define/public (vertex-xcoor) x-coor)

    (define code-for-color 0)
    (define code-for-type 0)
    (field [angle-at-vertex 'fail])
    
    (define/public (angle-for-points point1 point2)                                   ;;;;;;;ASSUMING ANTICLOCKWISE ORDER IN POINT1-POINT-POINT2
      (let* [(x1 (send point1 vertex-xcoor))
             (x2 (send point2 vertex-xcoor))
             (y1 (send point1 vertex-ycoor))
             (y2 (send point2 vertex-ycoor))
             (vector-1 (cons (- x-coor x1) (- y-coor y1)))
             (vector-2 (cons (- x-coor x2) (- y-coor y2)))
             (dot-product (+ (* (car vector-1) (car vector-2)) (* (cdr vector-1) (cdr vector-2))))
             (cross-product (- (* (car vector-1) (cdr vector-2)) (* (car vector-2) (cdr vector-1))))
             (mag-v1 (sqrt (+ (* (car vector-1) (car vector-1)) (* (cdr vector-1) (cdr vector-1)))))
             (mag-v2 (sqrt (+ (* (car vector-2) (car vector-2)) (* (cdr vector-2) (cdr vector-2)))))
             (some-angle (* 180 (/ 1 pi) (acos (/ dot-product (* mag-v1 mag-v2)))))
             (unit-vector-1 (cons (/ (- x-coor x1) mag-v1) (/ (- y-coor y1) mag-v1)))
             (unit-vector-2 (cons (/ (- x-coor x2) mag-v2) (/ (- y-coor y2) mag-v2)))
             (angle-bisector (cons (+ (/ (- x1 x-coor) mag-v1) (/ (- x2 x-coor) mag-v2)) (+ (/ (- y1 y-coor) mag-v1) (/ (- y2 y-coor) mag-v2))))]
        (if (> cross-product 0)
            (begin
              (set! angle-at-vertex (- 360 some-angle))
              (cond                ;;;;;;;;;;;;reflex angle case
                [(and (> y-coor y1) (> y-coor y2))
                 (begin (set! code-for-type 2) (- 360 some-angle))]
                [(and (< y-coor y1) (< y-coor y2))
               (begin (set! code-for-type 3) (- 360 some-angle))]
                ;[#t (- 360 some-angle)])
                [(> (car angle-bisector) 0)
                 (begin (set! code-for-type 5) (- 360 some-angle))]
                [#t
                 (begin (set! code-for-type 6) (- 360 some-angle))]))
            (begin
              (set! angle-at-vertex some-angle)
              (cond                ;;;;;;;;;;;;;;angle is not reflex
                [(and (> y-coor y1) (> y-coor y2))
                 (begin (set! code-for-type 1) some-angle)]
                [(and (< y-coor y1) (< y-coor y2))
               (begin (set! code-for-type 4) some-angle)]
                [(> (car angle-bisector) 0)
                 (begin (set! code-for-type 6) some-angle)]
                [#t
                 (begin (set! code-for-type 5) some-angle)])))))
      

    (define/public (change-color x)
      (set! code-for-color x))

    (define/public (get-color-code) code-for-color)

    (define/public (color)             ;;;so my idea is that color is not public, just internal thing, lamda whose argument would be decided through some function
      (define (color-type-helper code)                                      
        (cond [(= code 0) "Not Coloured Yet"]
              [(= code 1) "Red"]
              [(= code 2) "Blue"]
              [(= code 3) "Green"]
              [(= code 4) "color-x"]))
      (color-type-helper code-for-color))
    
    (define/public (vertex-type)
      (define (vertex-type-helper code)
      (cond
 ;;;;;;;;;;;;;;;;;;;;
        [(= code 1) "Start Vertex"]
        [(= code 2) "Split Vertex"]
        [(= code 3) "Merge Vertex"]
        [(= code 4) "End Vertex"]
        [(= code 5) "Right Regular Vertex"]
        [(= code 6) "Left Regular Vertex"]))
      (vertex-type-helper code-for-type))
    
    (field (is-special-vertex #f))  ;;;;;;;;;;;;; special vertex means it is required when connecting two trees of monotone

    (field [belongs-to-nodes 0])))
    ;(field [list-of-nodes '()])