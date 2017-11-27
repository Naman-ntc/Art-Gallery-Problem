#lang racket

(require "Polygon.rkt")
(require "vertex.rkt")
(require "edge.rkt")
(provide (all-defined-out))



(struct gnode (val list) #:transparent)



(define node%
  (class polygon%

    (define/public (fill-vec posn val)                               ;;; user can fill the vertex vector using a for loop from outside (didnt find constructor :P
      (vector-set! vec posn val))
    (super-new)
    (field [colored-count 0])
    (field [not-colored-yet vec]) ;;;not colored yet, vector of vertices, vec got from superclass
    (field [colors-possible (if (= size 3) '(1 2 3) '(1 2 3 4))])

    (inherit-field size)
    (inherit-field vec)
    ;(define/public (give-me-vec) vec)
    (define index 0)
    (define/public (set-index new-index)
      (set! index new-index))
    (define/public (get-index)
      index)

    (define/public (is-adjacent? other-node)
      (define vec1 (get-field vec other-node))
      (define bool1 #f)
      (define bool2 #f)
      (for [(i (list-upto size))]
        (for [(j (list-upto size))]
          (cond [(equal? (vector-ref vec i) (vector-ref vec1 j))
                 (if bool1 (set! bool2 #t) (set! bool1 #t))])))
      bool2)
    
    (define/public (node-update)
      (define indices-to-remove '()) ;;;to remove from  the vector not-colored-yet
      (define colors-to-remove '())  ;;;from colors possible
      (define (manipulate-not-colored-yet lst vect)
        (for ([i lst])
          (set! vect (vector-append (vector-take vect i) (vector-drop vect (+ i 1)))))
        (set! not-colored-yet vect))
      (define (manipulate-colors-possible lst)
        (for [(i lst)]
          (set! colors-possible (remove i colors-possible))))
                   
      (begin
        (for ([i (list-upto size)])
          (let [(code (send (vector-ref vec i) get-color-code))]
            (cond [(> code 0)
                   (begin
                     (set! colored-count (+ colored-count 1))
                     (set! indices-to-remove (cons i indices-to-remove))
                     (set! colors-to-remove (cons code colors-to-remove)))])))
        (manipulate-not-colored-yet (sort indices-to-remove >) not-colored-yet)
        (manipulate-colors-possible colors-to-remove)))))
  

    




    (define (color-a-graph graph)              ;;;;;;;;;graph has no cycles XD      
      (begin
        (color-node (gnode-val graph))
        (define length-list (length (gnode-list graph)))
        (define (helper l i)
          (cond [(< i length-list) (begin
                                     (color-a-graph (car l))
                                     (helper (cdr l) (+ i 1)))]))
        (helper (gnode-list graph) 0)))

    (define (color-node node)
      (send node node-update)
      (define colored-vertices-by-now (get-field colored-count node))  ;;;number
      (define node-size (get-field size node))
      (define count-vertices-to-be-colored (- node-size colored-vertices-by-now)) ;;;;number
      (define vector-vertices-to-be-colored (get-field not-colored-yet node))  ;;;;vector-of-vertices
      (define helper-vector-vertices (vector-map (λ(x) (cons (get-field belongs-to-nodes x) x)) vector-vertices-to-be-colored))


      (set! vector-vertices-to-be-colored
            (vector-map (λ(x) (cdr x))
                        (vector-sort
                         helper-vector-vertices
                         > 0 (vector-length helper-vector-vertices) #:key (lambda(x) (car x)))))      ;;;;;;;;;;;;according to priority :D

      (define color-priority '(2 1 3 4))

      (define colors-possible (get-field colors-possible node)) ;;;; colors-possible '(1 2 3 4) '(1 0 3 4)
  
      (set! colors-possible
            (let [(vector-of-possible (list->vector (filter (λ(y) (not (= (car y) 0))) (zip colors-possible color-priority))))]
              (map (λ(x) (car x)) (vector->list
                                   (vector-sort
                                    vector-of-possible
                                    > 0 (vector-length vector-of-possible) #:key (lambda(x) (cdr x))))))) 
      
      (cond
        [(= count-vertices-to-be-colored 4)
         (begin
           (for ([i '(0 1 2 3)])
             (send (vector-ref vector-vertices-to-be-colored i) change-color (list-ref colors-possible i))))]
           ;(set-field! node colored-count node-size)
           ;(set-field! node not-colored-yet (make-vector 0)))]
        [(= count-vertices-to-be-colored 3)
         (begin
           (for ([i '(0 1 2)])
             (send (vector-ref vector-vertices-to-be-colored i) change-color (list-ref colors-possible i))))]
           ;(set-field! node colored-count node-size)
           ;(set-field! node not-colored-yet (make-vector 0)))]
        [(= count-vertices-to-be-colored 2)
         (begin
           (for ([i '(0 1)])
             (send (vector-ref vector-vertices-to-be-colored i) change-color (list-ref colors-possible i))))]
           ;(set-field! node colored-count node-size)
           ;(set-field! node not-colored-yet (make-vector 0)))]
        [(= count-vertices-to-be-colored 1)
         (begin
           (define i 0)
           (send (vector-ref vector-vertices-to-be-colored i) change-color (list-ref colors-possible i)))]))
           ;(set-field! node colored-count node-size)
           ;(set-field! node not-colored-yet (make-vector 0)))]))