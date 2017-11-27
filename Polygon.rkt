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
  (define (a-foldr val)
    (foldr
     (λ(x y) (or y (if (= x val)
                       #t
                       y)))
     #f
     a-list))
  (and
   (a-foldr index-1)
   (a-foldr index-2)))


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
    (init-field vec)                                   ;;; vec is vetor of vertices, all members belong to vertex clas
    
    (field [edge-vec                                   ;;;edge-vec is vector of edges, all-members belong to edge class
            (vector-append
             (build-vector (- size 1)
                           (λ(i)
                             (make-object edge%
                               (vector-ref vec i)                  
                               (vector-ref vec (+ i 1))
                               i
                               (+ i 1))))
             (make-vector 1 (make-object edge%
                              (vector-ref vec (- size 1))
                              (vector-ref vec 0)
                              (- size 1)
                              0)))])

    (define/public (get-vector-of-vertices) vec)
    
    (define/public (vec-sorted-by-y-indices)                                         
      (vector-map                                                           
       (λ(x) (cdr x))                                       
       (vector-sort
        (list->vector (zip (vector->list vec) (list-upto size)))
        > 0 (vector-length vec) #:key (lambda(x) (send (car x) vertex-ycoor)))))

    (define (vector-of-angles)                                        ;;;  creates a vetor of angles from the coordinates 
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
     
      (define (do-this-for-that count-cases to-consider cases procedures i)
          (define consed-cases-procedures (zip cases procedures))
          (for [(arbit-var consed-cases-procedures)]
            (cond [(equal? (to-consider) (car arbit-var))
                   ((cdr arbit-var))])))
      
      (for [(i (vector->list (vec-sorted-by-y-indices)))]
        (define f1 (λ() (set! SLS (vector-append
                                 SLS
                                 (make-vector 1 (cons (vector-ref edge-vec i) (cons (vector-ref vec i) i)))))))

        (define f2 (λ() (let* [(position-of-e=>i-1 (search-for SLS (vector-ref edge-vec (- i 1))))           
                               (the-cdr-for-e=>i-1 (cdr (vector-ref SLS position-of-e=>i-1)))
                               (helper-of-e=>i-1  (car the-cdr-for-e=>i-1))
                               (position-of-helper-e=>i-1-in-vertex-vector (cdr the-cdr-for-e=>i-1))]       
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
                                                                position-of-helper-e=>i-1-in-vertex-vector))))  
                              (set! SLS (vector-append
                                         (vector-take SLS position-of-e=>i-1)               
                                         (vector-drop SLS (+ 1 position-of-e=>i-1)))))
                            (begin
                              (set! SLS (vector-append
                                         (vector-take SLS position-of-e=>i-1)                     
                                         (vector-drop SLS (+ 1 position-of-e=>i-1)))))))))

      (define f3 (λ() (let* [(index-of-nearest-edge (foldr-replace SLS vec i)) 
                             (nearest-edge-along-x (car (vector-ref SLS index-of-nearest-edge)))
                             (cdr-helper-of-nearest-left (cdr (vector-ref SLS index-of-nearest-edge)))
                             (helper-of-nearest-edge-along-x (car cdr-helper-of-nearest-left))
                             (position-of-helper-in-vertex-vector (cdr cdr-helper-of-nearest-left))]
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
                          (vector-set! SLS
                                       index-of-nearest-edge
                                       (cons (car (vector-ref SLS index-of-nearest-edge)) (cons (vector-ref vec i) i))) 
                          (set! SLS (vector-append
                                     SLS
                                     (make-vector 1 (cons (vector-ref edge-vec i) (cons (vector-ref vec i) i)))))))))

      (define f4-2 (λ() (let* [(index-of-nearest-edge (foldr-replace SLS vec i))
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
                              (vector-set! SLS index-of-nearest-edge (cons (car (vector-ref SLS index-of-nearest-edge)) (cons (vector-ref vec i) i)))))))

      (define f4 (λ() (begin (f3) (f4-2))))
      (define f5 (λ() (f4-2)))
      (define f6 (λ() (f1) (f2)))
        (do-this-for-that 6
                          (λ() (send (vector-ref vec i) vertex-type))
                          (list "Start Vertex" "End Vertex" "Split Vertex" "Merge Vertex" "Right Regular Vertex" "Left Regular Vertex")
                          (list f1 f2 f3 f4 f5 f6) i))) 
      
    
    (field [piece-wise-polygons (make-vector 1)]) 
    
    (define/public (refresh-piecewise)                                           
      (set! piece-wise-polygons (vector-append
                                 (make-vector 1 (list-upto size))
                                 (make-vector (vector-length diagonals)))))  

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
      
    
    (define/public (procedure)                                                    
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

    
    (define/public (procedure1)
      (set! piece-wise-polygons
            (vector-map
             (λ(x) (list->vector (map (λ(y) (vector-ref vec y)) x))) piece-wise-polygons)))))
