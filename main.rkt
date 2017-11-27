#lang racket

(require data/gvector)
(require "vertex.rkt")
(require "edge.rkt")
(require "Polygon.rkt")
(require "coloring.rkt")

(provide (all-defined-out))
(define (vector-append* vector)
  (list->vector (append* (map (λ(x) (vector->list x)) (vector->list vector)))))

;;;;gnodify take vector of mcons pairs in which  mcar is the node and the mcdr is the gvector of the nodes connected to it.
;;;;


(define final-structure '())

(define (gnodify complex-data-structure)

  (define complex-length (vector-length complex-data-structure))
  (define (check-if-special-node a-node)
    (define size (get-field size a-node))
    (define vec (get-field vec a-node))
    (define special-vec (vector-map (λ (i) (get-field is-special-vertex i)) vec))
    (define bool #f)
    (if (and (vector-ref special-vec (- size 1))
             (vector-ref special-vec 0))
        (set! bool #t)
        (for [(i (list-upto (- size 1)))]
          (cond [(and (vector-ref special-vec i)
                      (vector-ref special-vec (+ i 1)))
                 (set! bool #t)])))
    bool)

  (define special-nodes
    (vector-map
     (λ(xx)
       (define special-nodes (make-gvector))
       (for [(i (vector-length xx))]
         (let* [(m-pair (vector-ref xx i))
               (car-of-mpair (mcar m-pair))]  ;;;car-of-mpair is a node right XD
           (cond [(check-if-special-node car-of-mpair) (gvector-add! special-nodes (cons car-of-mpair i))])))
       special-nodes)
     complex-data-structure))
  
  (for [(i (list-upto complex-length))]
    (define gvector1 (vector-ref special-nodes i))
    (define lgvector1 (gvector-count gvector1))
    (for [(j (list-upto complex-length))
          #:unless (= i j)]
      (define gvector2 (vector-ref special-nodes j))
      (define lgvector2 (gvector-count gvector2))
      (for [(k (list-upto lgvector1))]
        (for [(l (list-upto lgvector2))]
          (let* [(gref1 (gvector-ref gvector1 k))
                 (gref2 (gvector-ref gvector2 l))
                 (node1 (car gref1))
                 (node2 (car gref2))]
                 (cond [(send node1 is-adjacent? node2)
                        (gvector-add! (mcdr (vector-ref (vector-ref complex-data-structure i) (cdr gref1)))
                                      (mcar (vector-ref (vector-ref complex-data-structure j) (cdr gref2))))]))))))
  
  (set! final-structure (vector-append* complex-data-structure))
  (define final-length (vector-length final-structure))
  
  (for [(i (list-upto final-length))]
    (send (mcar (vector-ref final-structure i)) set-index i))

  (define x (random final-length))

  (define (create-gnode index mother)
    (define mpair (vector-ref final-structure index))
    (define node (mcar mpair))
    (define gvector (mcdr mpair))
    (define gvector-length (gvector-count gvector))
    (define gnode-list '())
    (for [(i (list-upto gvector-length))
          #:unless (equal? mother (gvector-ref gvector i))]
      (set! gnode-list (cons (create-gnode (send (gvector-ref gvector i) get-index) node) gnode-list)))
    (gnode node gnode-list))
  (create-gnode x #f))
  ;final-structure)