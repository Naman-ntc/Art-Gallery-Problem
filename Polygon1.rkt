#lang racket

;(require "coloring.rkt")
(require "vertex.rkt")
(require "edge.rkt")
;(require "monotone.rkt")
(provide (all-defined-out))

(define (zip l1 l2)                                         ;;;;;;;;;;;;;;;check it please :)        the normal zip-function 
  (if (or (null? l1) (null? l2))
      '()
      (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define (list-upto n)                                 ;;;;;;; (list-upto 2) ==> '(0 1)
  (if (= n 0)
      '()
      (append (list-upto (- n 1)) (list (- n 1)))))

(define (push stack x)
  (set! x (cdr x)))

(define (pop stack x)
  (set! x (cdr x)))

;;;;;;;;;An important note                           ###########################################################################3
;> (define p%                                         #######################################################
;    (class object%
;      (init a) 
;      (super-new)))
;> (define p1 (new p% [a 2]))
;> (define p2 (new p% [a 2]))
;> (eq? p1 p2)                                    ########################################################################################3
;#f
;> (equal? p1 p2)
;#f
;> (define p2 p1)                                  ########################################################################333
;> (eq? p1 p2)
;#t
;> (equal? p1 p2)                          3######################################################################
;#t


(define (search-for some-vector some-sort-of-car)            ;;;;;;;;;;;; given some vector with each element some sort of pair
  (define (helper i)                                 ;;;;;;;;;;;;; This stuff returns the index in the vector of such a pair with (car vector) ==> some-sort-of-car
    (if (equal? (car (vector-ref some-vector i)) some-sort-of-car)
        i
        (helper (+ i 1))))
  (helper 0))



(define (lie-between---breaking-polygons a-list index-1 index-2)
  (and
   (foldr
    (λ(x y) (or y (if (= x index-1)
                      #t
                      y)))
    #f
    a-list)
   (foldr
    (λ(x y) (or y (if (= x index-2)
                      #t
                      y)))
    #f
    a-list)))



(define (foldr-replace SLS vec j)
  (define min-till-now (expt 10 5))
  (define ans 0)
  (define index 0)
  (for [(i (vector->list SLS))]
    (cond [(< (send (car i)  distance-from-edge-along-x-direction (vector-ref vec j)) min-till-now)
           (set! min-till-now (send (car i)  distance-from-edge-along-x-direction (vector-ref vec j)))(set! ans index)])
    (set! index (+ index 1)))
  ans)






(define polygon%
  (class object%
    (super-new)
    (init-field size)
    (init-field vec)                                   ;;; vec is vetor of vertices, all members belong to vertex class
    ;(field [vec (make-vector size)])
    (define/public (get-vector-of-vertices)                                            ;;;we have access to vector outside the class using it.
      vec)

    ;(define/public (fill-vec posn val)                               ;;; user can fill the vertex vector using a for loop from outside (didnt find constructor :P
    ;  (vector-set! vec posn val))

    (field [edge-vec                               ;###############################################################
            (vector-append
             (build-vector (- size 1)
                           (λ(i)
                             (make-object edge%
                               (vector-ref vec i)                  ;;;;########################### check order
                               (vector-ref vec (+ i 1))
                               i
                               (+ i 1))))
             (make-vector 1 (make-object edge%
                              (vector-ref vec (- size 1))
                              (vector-ref vec 0)
                              (- size 1)
                              0)))])

    ;;;;;;;;;;;;;;;change the vector such that higest index is present at top
    
    (define/public (vec-sorted-by-y-indices)                                         ;;;  we need vertices sorted by y coordinate so there it is. Actualy it is
      (vector-map                                                            ;;;;;;;;; index of the vectors in the vector vec
       (λ(x) (cdr x))                                        ;;;;; Contact 8291446857
       (vector-sort
        (list->vector (zip (vector->list vec) (list-upto size)))
        > 0 (vector-length vec) #:key (lambda(x) (send (car x) vertex-ycoor)))))

    (define/public (vector-of-angles)                                        ;;;  created a vetor of angles from the coordinates, (angles function  is to be defined
      (define n size)
      (vector-append
       (make-vector 1 (send (vector-ref vec 0)
                            angle-for-points
                            (vector-ref vec (- n 1))
                            (vector-ref vec 1)))
       (build-vector (- n 2)
                     (lambda(x) (send (vector-ref vec (+ x 1))
                                      angle-for-points
                                      (vector-ref vec x)
                                      (vector-ref vec (+ x 2)))))
       (make-vector 1 (send (vector-ref vec (- n 1))
                            angle-for-points
                            (vector-ref vec (- n 2))
                            (vector-ref vec 0)))))

    (define angles-vector (vector-of-angles))
    
    (define/public (polygon-type)                                             ;;;  to check which polygon is it
      ;(define list-angles (vector->list vector-of-angles))                  ;;;  list-angles is list of the angles converted from vector of angles
      (define n size)                                                       ;;;  :P
      ;;      (define vector-of-side-lengths                                            ;;; created coz thought would need it
      ;;        (vector-append
      ;;         (build-vector (- n 1)
      ;;                       (λ(x) (side-length (vector-ref vec x)
      ;;                                     (vector-ref vec (+ x 1)))))
      ;;         (make-vector 1 (side-length (vector-ref vec (- n 1)) (vector-ref vec 0)))))
      ;;     
      (λ(xx) (cond [(= xx 1)                                                   ;;;  ((polygon-type) 1) checkes if it is an orthogonal polygon
                    (for/and
                        ([v1 (vector->list (vector-of-angles))])
                      (or (equal? v1 90) (equal? v1 270)))]

                   [(= xx 2)                                                   ;;;  ((polygon-type) 2) checkes if it is an convex polygon polygon
                    ;;;;;procedure to check if it is regular!!!, would checking b(car l1)y angles give it a mismatch :
                    (for/and
                        ([v1 (vector->list (vector-of-angles))])
                      (v1 . < . 180))]  ;;; URL https://docs.racket-lang.org/guide/for.html Section 11.5 


                   ;;;;...... other types can be appended h
                   )))



    (define diagonals (make-vector 0))
    (define/public (get-diagonals) diagonals)
    
    (define/public (break-to-monotone)
      (define SLS (make-vector 0))
      (for ([i (vector->list (vec-sorted-by-y-indices))])
        (cond

          [(equal?
            (send (vector-ref vec i) vertex-type)   ;; vec is ###########PROCESSED#################### vector of vertices
            "Start Vertex")
           (begin (set! SLS (vector-append
                             SLS
                             (make-vector 1 (cons (vector-ref edge-vec i) (cons (vector-ref vec i) i))))))]; (display SLS))]        ;; edge-vec is vector of edges



          [(equal?
            (send (vector-ref vec i) vertex-type)
            "End Vertex")
           (let* [(position-of-e=>i-1 (search-for SLS (vector-ref edge-vec (- i 1))))           ;;;;;;;returns the index of e=>i-1 in SLS
                  (the-cdr-for-e=>i-1 (cdr (vector-ref SLS position-of-e=>i-1)))
                  (helper-of-e=>i-1  (car the-cdr-for-e=>i-1))
                  (position-of-helper-e=>i-1-in-vertex-vector (cdr the-cdr-for-e=>i-1))]       ;;;;;;;Check CORRECTNESS of search function, check indices order specially
             (if (equal?
                  (send helper-of-e=>i-1 vertex-type)
                  "Merge Vertex")
                 (begin
                   (set-field! is-special-vertex (vector-ref vec i) #t)
                   (set-field! is-special-vertex helper-of-e=>i-1 #t)
                   (set! diagonals (vector-append
                                    diagonals
                                    (make-vector 1 (make-object edge%
                                                     (vector-ref vec i)
                                                     helper-of-e=>i-1
                                                     i
                                                     position-of-helper-e=>i-1-in-vertex-vector)))) ;(display SLS) 
                   (set! SLS (vector-append
                              (vector-take SLS position-of-e=>i-1)               ;;;;;;;;;;;;;Check correctness ######################################
                              (vector-drop SLS (+ 1 position-of-e=>i-1))))); (display SLS))             ;;;;;;;;;;;;;;Check correctnesss ####################################
                 (begin
                   (set! SLS (vector-append
                              (vector-take SLS position-of-e=>i-1)                     ;;;;;;;;;;;;;Check correctness ######################################
                              (vector-drop SLS (+ 1 position-of-e=>i-1)))))))]; (display SLS))]         ;;;;;;;;;;;;;;Check correctnesss ####################################



          [(equal?
            (send (vector-ref vec i) vertex-type)
            "Split Vertex")
           (let* [(index-of-nearest-edge (foldr-replace SLS vec i)) ;;;THIS IS CORRECT INDEX ASSUMING FOLDR WAS RIGHT
                  (nearest-edge-along-x (car (vector-ref SLS index-of-nearest-edge)))
                  (cdr-helper-of-nearest-left (cdr (vector-ref SLS index-of-nearest-edge)))
                  (helper-of-nearest-edge-along-x (car cdr-helper-of-nearest-left))
                  (position-of-helper-in-vertex-vector (cdr cdr-helper-of-nearest-left))]
             (begin
               ;(display index-of-nearest-edge)
               (display (vector-map (λ(x) (cons (get-field index-first-vertex-in-vertex-vector (car x)) (cddr x))) SLS)) (display "                ")
               
               (display "Split") ;(display index-of-nearest-edge-from-the-back )
               (display index-of-nearest-edge) 
               (set-field! is-special-vertex (vector-ref vec i) #t)
               (set-field! is-special-vertex helper-of-nearest-edge-along-x #t)
               (set! diagonals (vector-append
                                diagonals
                                (make-vector 1 (make-object edge%
                                                 (vector-ref vec i)
                                                 helper-of-nearest-edge-along-x
                                                 i
                                                 position-of-helper-in-vertex-vector))))       
               (vector-set! SLS
                            index-of-nearest-edge
                            (cons (car (vector-ref SLS index-of-nearest-edge)) (cons (vector-ref vec i) i))) 
               (set! SLS (vector-append
                          SLS
                          (make-vector 1 (cons (vector-ref edge-vec i) (cons (vector-ref vec i) i)))))))]; (display SLS)))]



          [(equal?
            (send (vector-ref vec i) vertex-type)
            "Merge Vertex")
          
                  
           (begin
             (let* [(position-of-e=>i-1 (search-for SLS (vector-ref edge-vec (- i 1))))           ;;;;;;;returns the index of e=>i-1 in SLS
                    (the-cdr-for-e=>i-1 (cdr (vector-ref SLS position-of-e=>i-1)))
                    (helper-of-e=>i-1  (car the-cdr-for-e=>i-1))
                    (position-of-helper-e=>i-1-in-vertex-vector (cdr the-cdr-for-e=>i-1))
                    (index-of-nearest-edge (foldr-replace SLS vec i))
                    (nearest-edge-along-x (car (vector-ref SLS index-of-nearest-edge)))
                    (cdr-helper-of-nearest-left (cdr (vector-ref SLS index-of-nearest-edge)))
                    (helper-of-nearest-edge-along-x (car cdr-helper-of-nearest-left))
                    (position-of-helper-in-vertex-vector (cdr cdr-helper-of-nearest-left))]
               (if (equal?
                    (send helper-of-e=>i-1 vertex-type)
                    "Merge Vertex")
                   (begin
                     (set-field! is-special-vertex (vector-ref vec i) #t)
                     (set-field! is-special-vertex helper-of-e=>i-1 #t)
                     (set! diagonals (vector-append
                                      diagonals
                                      (make-vector 1 (make-object edge%
                                                       (vector-ref vec i)
                                                       helper-of-e=>i-1
                                                       i
                                                       position-of-helper-in-vertex-vector)))) 
                     (set! SLS (vector-append
                                (vector-take SLS position-of-e=>i-1)
                                (vector-drop SLS (+ 1 position-of-e=>i-1))))); (display SLS))
                   (begin
                     (set! SLS (vector-append
                                (vector-take SLS position-of-e=>i-1)
                                (vector-drop SLS (+ 1 position-of-e=>i-1))))))); (display SLS))
             (let* [(index-of-nearest-edge (foldr-replace SLS vec i))
                    (nearest-edge-along-x (car (vector-ref SLS index-of-nearest-edge)))
                    (cdr-helper-of-nearest-left (cdr (vector-ref SLS index-of-nearest-edge)))
                    (helper-of-nearest-edge-along-x (car cdr-helper-of-nearest-left))
                    (position-of-helper-in-vertex-vector (cdr cdr-helper-of-nearest-left))]
               (if (equal?
                    (send helper-of-nearest-edge-along-x vertex-type)
                    "Merge Vertex")
                   (begin
                     (set-field! is-special-vertex (vector-ref vec i) #t)
                     (set-field! is-special-vertex helper-of-nearest-edge-along-x #t)
                     (set! diagonals (vector-append
                                      diagonals
                                      (make-vector 1 (make-object edge%
                                                       (vector-ref vec i)
                                                       helper-of-nearest-edge-along-x
                                                       i
                                                       position-of-helper-in-vertex-vector))))
                     (vector-set! SLS index-of-nearest-edge (cons (car (vector-ref SLS index-of-nearest-edge)) (cons (vector-ref vec i) i))) (display SLS))
                   (vector-set! SLS index-of-nearest-edge (cons (car (vector-ref SLS index-of-nearest-edge)) (cons (vector-ref vec i) i))))))]



          [(equal?
            (send (vector-ref vec i) vertex-type)
            "Right Regular Vertex")
           (let* [(index-of-nearest-edge (foldr-replace SLS vec i))
                  (nearest-edge-along-x (car (vector-ref SLS index-of-nearest-edge)))
                  (cdr-helper-of-nearest-left (cdr (vector-ref SLS index-of-nearest-edge)))
                  (helper-of-nearest-edge-along-x (car cdr-helper-of-nearest-left))
                  (position-of-helper-in-vertex-vector (cdr cdr-helper-of-nearest-left))]
             (if (equal?
                  (send helper-of-nearest-edge-along-x vertex-type)
                  "Merge Vertex")
                 (begin
                   
                   (set-field! is-special-vertex (vector-ref vec i) #t)
                   (set-field! is-special-vertex helper-of-nearest-edge-along-x #t)
                   (set! diagonals (vector-append
                                    diagonals
                                    ;                                    (make-vector 1 (new edge%
                                    ;                                                        [first-vertex (vector-ref vec i)]
                                    ;                                                        [second-vertex helper-of-nearest-edge-along-x]
                                    ;                                                        [index-first-vertex-in-vertex-vector i]
                                    ;                                                        [index-first-vertex-in-vertex-vector position-of-helper-in-vertex-vector]))))
                                    (make-vector 1 (make-object edge% (vector-ref vec i) helper-of-nearest-edge-along-x i position-of-helper-in-vertex-vector))))
                   (vector-set! SLS index-of-nearest-edge (cons (car (vector-ref SLS index-of-nearest-edge)) (cons (vector-ref vec i) i))))
                 (begin ;(display index-of-nearest-edge-from-the-back )
                   (display index-of-nearest-edge) (display (vector-map (λ(x) (cons (get-field index-first-vertex-in-vertex-vector (car x)) (cddr x))) SLS)) (display "                           ") (display "right") (display position-of-helper-in-vertex-vector) (vector-set! SLS index-of-nearest-edge (cons (car (vector-ref SLS index-of-nearest-edge)) (cons (vector-ref vec i) i))))))]



          [(equal?
            (send (vector-ref vec i) vertex-type)
            "Left Regular Vertex")
           (begin
             (let* [(position-of-e=>i-1 (search-for SLS (vector-ref edge-vec (- i 1))))           ;;;;;;;returns the index of e=>i-1 in SLS
                    (the-cdr-for-e=>i-1 (cdr (vector-ref SLS position-of-e=>i-1)))
                    (helper-of-e=>i-1  (car the-cdr-for-e=>i-1))
                    (position-of-helper-e=>i-1-in-vertex-vector (cdr the-cdr-for-e=>i-1))]       ;;;;;;;Check CORRECTNESS of search function, check indices order specially
               (if (equal?
                    (send helper-of-e=>i-1 vertex-type)
                    "Merge Vertex")
                   (begin
                     (set-field! is-special-vertex (vector-ref vec i) #t)
                     (set-field! is-special-vertex helper-of-e=>i-1 #t)
                     (set! diagonals (vector-append diagonals
                                                    (make-vector 1
                                                                 (make-object edge%
                                                                   (vector-ref vec i)
                                                                   helper-of-e=>i-1
                                                                   i
                                                                   position-of-helper-e=>i-1-in-vertex-vector))))
                     (set! SLS (vector-append
                                (vector-take SLS position-of-e=>i-1)               ;;;;;;;;;;;;;Check correctness ######################################
                                (vector-drop SLS (+ 1 position-of-e=>i-1)))))             ;;;;;;;;;;;;;;Check correctnesss ####################################
                   (set! SLS (vector-append
                              (vector-take SLS position-of-e=>i-1)                     ;;;;;;;;;;;;;Check correctness ######################################
                              (vector-drop SLS (+ 1 position-of-e=>i-1))))))         ;;;;;;;;;;;;;;Check correctnesss ##############################
             (set! SLS (vector-append SLS (make-vector 1 (cons (vector-ref edge-vec i) (cons (vector-ref vec i) i))))))])))

    
    ;(break-to-monotone)
    (field [piece-wise-polygons (make-vector 1)]) ;;;;;{this is a list of indices of polygon's vertices. Total + 1 diag  polygons are created}
    ;(vector-set! piece-wise-polygons 0 (list-upto size))
    (define/public (refresh-piecewise)
      (set! piece-wise-polygons (vector-append (make-vector 1 (list-upto size)) (make-vector (vector-length diagonals)))))

    (define/public (break-the-list-and-set! a-list first second where1 where2)
      (define a-vector (list->vector a-list))
      (define pos1 #f)
      (define pos2 #f)
      (define count 0)
      (for ([i a-list])
        (begin
          (cond [(= i first) (set! pos1 count)])
          (cond [(= i second) (set! pos2 count)])
          (set! count (+ count 1))))
      (cond [(> pos1 pos2) (begin (set! pos2 (+ pos1 pos2))
                                  (set! pos1 (- pos2 pos1))
                                  (set! pos2 (- pos2 pos1)))])
      (define vector1 (vector-take a-vector (+ pos1 1)))
      (define vector2 (vector-drop a-vector pos2))
      (define vector3 (vector-take (vector-drop a-vector  pos1 ) (+ 1 (- pos2 pos1))))
      (vector-set! piece-wise-polygons where1 (append (vector->list vector1) (vector->list vector2)))
      (vector-set! piece-wise-polygons where2 (vector->list vector3)))
      
    
    (define/public (procedure)                                                     ;;;assuming diagonals is correct in orientations
      (display (vector-map (λ(x) (cons (get-field index-first-vertex-in-vertex-vector x) (get-field index-second-vertex-in-vertex-vector x))) diagonals))
      (for ([i (list-upto (vector-length diagonals))])
        (for ([j (list-upto (+ i 1))])
          (let* [(a-list (vector-ref piece-wise-polygons j))
                 (diagonal (vector-ref diagonals i))
                 (first-index (get-field index-first-vertex-in-vertex-vector diagonal))
                 (second-index (get-field index-second-vertex-in-vertex-vector diagonal))]
            (cond [(lie-between---breaking-polygons
                    a-list
                    first-index
                    second-index)
                   (break-the-list-and-set! a-list first-index second-index (+ i 1) j)])))))

    ;(procedure)
    ;(display piece-wise-polygons)
    (define/public (procedure1)
      (set! piece-wise-polygons
            (vector-map
             (λ(x) (list->vector (map (λ(y) (vector-ref vec y)) x))) piece-wise-polygons)))))
