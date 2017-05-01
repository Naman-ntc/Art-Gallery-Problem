#lang racket

(require "vertex.rkt")
(require "edge.rkt")
(require "Polygon.rkt")
(require "coloring.rkt")
(require data/gvector)
(provide (all-defined-out))

(provide (all-defined-out))


 


(define monotone%
  (class polygon%
    (super-new)
    (inherit-field size vec)
    (inherit-field edge-vec)
    
    (define (dot-product vec1 vec2)                   ;;; subroutine for check-diagonal
      (let* [(point1 (car vec1))
             (point2 (cadr vec1))
             (point3 (car vec2))
             (point4 (cadr vec2))
             (diff-x1 (- (car point2) (car point1)))
             (diff-y1 (- (cadr point2) (cadr point1)))
             (diff-x2 (- (car point4) (car point3)))
             (diff-y2 (- (cadr point4) (cadr point3)))]
        (+ (* diff-x1 diff-x2) (* diff-y1 diff-y2))))



    (define (rotate-left vec)                          ;;; subroutine for check-diagonal
      (let* [(point1 (car vec))
             (point2 (cadr vec))
             (x1 (car point1))
             (y1 (cadr point1))
             (x2 (car point2))
             (y2 (cadr point2))]
        (list (list y2 x1) (list y1 x2))))
    

    (define (neighbours vertex)   
      (define (helper neighbouring-vertices)
        (if (equal? (cadr (car neighbouring-vertices)) vertex) (car neighbouring-vertices) (helper (cdr neighbouring-vertices))))
      (helper (triplet-list vec)))                 ;; triplet-vec returns a list


    (define (check-diagonal1 vertex1 vertex2)

  (define (sign vertex vertex1 vertex2)
  (let* [    (x1 (get-field x-coor vertex1))
             (y1 (get-field y-coor vertex1))
             (x2 (get-field x-coor vertex2))
             (y2 (get-field y-coor vertex2))
             (x3 (get-field x-coor vertex))
             (y3 (get-field y-coor vertex))]
    (cond [#t 
           (let* [(m1 (/ (- y2 y1) (- x2 x1)))
                     
                      (c1 (- y1 (* m1 x1)))
                      (ans (- y3 (+ (* m1 x3) c1)))]
             (cond [(> ans 0) '(1)]
                   [(< ans 0) '(0)]
                   [#t '()]))])))
  (define a (append* (vector->list (vector-map (lambda (x) (sign x vertex1 vertex2)) vec))))
  (display a)
  (if (or (foldr (lambda (x y) (and y (= x 0))) #t  a) (foldr (lambda (x y) (and y (= x 1))) #t  a)) #f #t)) 
  
  

(define (between x0 x1 x2)
  (let [(x3 (min x1 x2))
        (x4 (max x1 x2))]
    (if (and (> x0 x3) (< x0 x4)) #t #f)))
        


    (define (intersect? vertex3 vertex4 edge2) 
      (let* [(vertex1 (get-field first-vertex edge2))
             (vertex2 (get-field second-vertex edge2))
             (x1 (get-field x-coor vertex1))
             (y1 (get-field y-coor vertex1))
             (x2 (get-field x-coor vertex2))
             (y2 (get-field y-coor vertex2))
             (x3 (get-field x-coor vertex3))
             (y3 (get-field y-coor vertex3))
             (x4 (get-field x-coor vertex4))
             (y4 (get-field y-coor vertex4))]
        (cond [(and (not (equal? x1 x2)) (not (equal? x3 x4)))
               (let* [(m1 (/ (- y2 y1) (- x2 x1)))
                      (m2 (/ (- y4 y3) (- x4 x3)))
                      (c1 (- y1 (* m1 x1)))
                      (c2 (- y3 (* m2 x3)))]
                 (if (equal? m1 m2) #f  
                  (let[ (x0 (/ (- c2 c1) (- m1 m2)))]
                    (if (and (between x0 x1 x2) (between x0 x3 x4)) #t #f))))]
              [(and (equal? x1 x2) (not (equal? x3 x4)))
               (let* [
                      (m2 (/ (- y4 y3) (- x4 x3)))
                      
                      (c2 (- y3 (* m2 x3)))
                      (y0 (+ (* m2 x1) c2))]
                 (if (and (between y0 y1 y2) (between y0 y3 y4)) #t #f))]
              [(and (not (equal? x1 x2)) (equal? x3 x4))
               (let* [(m1 (/ (- y2 y1) (- x2 x1)))
                      
                      (c1 (- y1 (* m1 x1)))
                     
                      (y0 (+ (* m1 x3) c1))]
                 (if (and (between y0 y1 y2) (between y0 y3 y4)) #t #f))]
              [#t #f])))
              


      (define (check-diagonal vertex1 vertex2)
        (and (check-diagonal1 vertex1 vertex2) (foldr
         (λ(x y) (and y (not (intersect? vertex1 vertex2 x))))
         #t
         (vector->list edge-vec))))

    
    
                      
             
             
                      
      



    (define (rotate-list lst)
      (append (cdr lst) (list (car lst))))
    (define (process-input1 lst)              ;process-imput1 accepts list
      (define (helper lst optimal-pair)
        (if (equal? optimal-pair (car lst)) lst (helper (rotate-list lst) optimal-pair)))
      (let* [(optimal-pair  (foldr (lambda (x y) (cond [(> (get-field y-coor x) (get-field y-coor y)) x]
                                                       [(= (get-field y-coor x) (get-field y-coor y)) (if (< (get-field x-coor x) (get-field x-coor y)) x y)]
                                                       [#t y]))  (car lst) lst))]
        (helper lst optimal-pair)))
    (define the-final-list-for-triangulater (process-input1 (vector->list vec)))
    (define/public (triangulate-it) (vector-map (λ(x) (mcons (mcar x) (vector->gvector (mcdr x))))
                                                (make-typical-data-structure (triangulater the-final-list-for-triangulater)))) 
    (define/public (triangulater final-list-of-vertices)                   ;;final list of vertices is the processed vertex
      (if (= 3 (length final-list-of-vertices))
          (make-triangle final-list-of-vertices)              ;;make-triangle makes nodes
          (let*[(pair-triplet1 (pick-three-vertices 'left-tringle   final-list-of-vertices))      ;;X-triangle function returns a pair of vector of three vertices in y order and vertex to be exculded otherwise #f
                (pair-triplet2 (pick-three-vertices 'right-tringle  final-list-of-vertices))      ;;we define a trim polygon function
                (pair-triplet3 (pick-three-vertices 'middle-tringle final-list-of-vertices))
                (valid-triangle (The-chosen-one pair-triplet3 pair-triplet1 pair-triplet2))                                                                    ;;
                (The-node (make-node valid-triangle))
                (remaining-polygon (trim-polygon valid-triangle final-list-of-vertices))
                (vector-of-triangles (triangulater remaining-polygon))]
            (vector-append The-node vector-of-triangles))
          ))
              
   

    (define (The-chosen-one triangle1 triangle2 triangle3)              ;;valid? checks whether the third edge is in the polygon triangle is the pair triplet
      (cond [(valid? triangle1)  triangle1] 
            [(valid? triangle2) triangle2] 
            [#t triangle3] ))
               
     
    (define (valid? triangle)
      (let*[(lonely-edge (cdr triangle))
            (list-of-three-vertices (car triangle))
            (two-vertices-of-diagonals (remove lonely-edge list-of-three-vertices))]
        (check-diagonal (car two-vertices-of-diagonals) (cadr two-vertices-of-diagonals))))
             

    (define (pick-three-vertices instruction collection)              ;;returns pair of vector of three vertices in y order and vertex to be exculded or #f if not possible instruction is teh sym
      (cond [(eq?  instruction 'left-tringle)
             (let* [(start-vertex (car collection))
                    (next-two-vertex (two-vertex 'left (cdr collection)))   ;; its a list of two vertexes in order counterclockwise               
                    (three-vertices (cons start-vertex next-two-vertex))]
               (cons three-vertices (cadr three-vertices)))]
            [(eq?  instruction 'right-tringle)
             (let* [(start-vertex (car collection))
                    (next-two-vertex (two-vertex 'right (cdr collection)))   ;; its a list of two vertexes in order counterclockwise             
                    (three-vertices (cons start-vertex next-two-vertex))]
               (cons three-vertices (cadr three-vertices)))]
            [(eq?  instruction 'middle-tringle)
             (let* [(start-vertex (car collection))
                    (next-two-vertex (two-vertex 'left-right (cdr collection)))   ;; its a pair of two vertexes in order counterclockwise           
                    (three-vertices (cons start-vertex next-two-vertex))]
               (cons three-vertices (car three-vertices)))]))
             
                     
    (define (filter p l)
      (if (p (car l)) (cons (car l) (filter p (cdr l))) (filter p (cdr l)))) 


    (define (two-vertex instruction collection)         ;; collection is the final list of vertexes                           ;; returns two-vertices or false if two vertices cant be selected.
      (cond [(eq? instruction 'left) (list (car collection) (cadr collection))]
            [(eq? instruction 'right)
             (let [(collection1 (reverse collection))] (list (car collection1) (cadr collection1)))] 
            [(eq? instruction 'left-right) (list (car collection) (car (reverse collection)))]))
              


    (define (trim-polygon triangle polygon)                                ;;trim-polygon returns another final-list-of-vertexs with topmost at start and same properties by removing triangle
      (let [(vertex1 (cdr triangle))]
        (process-input1 (remove vertex1 polygon))))      

    (define (make-triangle list-of-vertices)                                ;; here the list isnt consed 
      (let* [(ordered-list-of-three-vertices (make-counterclockwise list-of-vertices))]
        (make-vector 1 (make-object node% 3 (list->vector ordered-list-of-three-vertices)))))  
              
    ;the base case returns the vector of the node                            
    (define (make-node triangle)                                          ;;triangle is basically the triplet its not consed dont forget that returns the vector of the node                         
      (let* [(list-of-three-vertices (car triangle))
             (ordered-list-of-three-vertices (make-counterclockwise list-of-three-vertices))]
        (make-vector 1 (make-object node% 3 (list->vector ordered-list-of-three-vertices)))))

    (define (make-counterclockwise list-of-three-vertices)
      (if (determinant list-of-three-vertices) list-of-three-vertices
          (let* [(first (car list-of-three-vertices))
                 (second(cadr list-of-three-vertices))
                 (third (caddr list-of-three-vertices))]
            (list first third second))))

    (define (determinant list-of-three-vertices)
      (let* [(first (car list-of-three-vertices))
             (second(cadr list-of-three-vertices))
             (third (caddr list-of-three-vertices))
             (x1 (get-field x-coor first))
             (y1 (get-field y-coor first))
             (x2 (get-field x-coor second))
             (y2 (get-field y-coor second))
             (x3 (get-field x-coor third))
             (y3 (get-field y-coor third))]
        (> 0 (- (* (- x1 x2) (- y2 y3)) (* (- x2 x3) (- y1 y2))))))

    (define (triplet-list vec)     ;; vec is the vector of vertices it gives a list of triplets 
      (define (make-triplets l1 l2 l3)
        (if (null? l1) '()
            (cons (list (car l1) (car l2) (car l3)) (make-triplets (cdr l1) (cdr l2) (cdr l3))))) 
      (let* [(list-vertices (vector->list vec))
             (rotate-left1 (rotate-list list-vertices))
             (rotate-right1 (reverse (rotate-list (reverse list-vertices))))]
        (make-triplets rotate-right1 list-vertices rotate-left1)))

    (define (make-typical-data-structure vec-of-nodes)     ;;l is the vector of nodes i return a list of pairs of nodes consed with vector of adjacent nodes  
      (if (= (vector-length vec-of-nodes) 1)
          (vector (mcons (vector-ref vec-of-nodes 0) (make-vector 0))) 
          (let* [(start (mcons (vector-ref vec-of-nodes 0) (vector (vector-ref vec-of-nodes 1))))
                 (end   (mcons (vector-ref vec-of-nodes (- (vector-length vec-of-nodes) 1)) (vector (vector-ref vec-of-nodes (- (vector-length vec-of-nodes) 2)))))
                 (vec1  (vector-drop-right vec-of-nodes 2))
                 (vec2  (vector-drop vec-of-nodes 2))
                 (vec3  (vector-drop-right (vector-drop vec-of-nodes 1) 1))
                 (vec4  (build-combined-vec vec1 vec2))
                 (vec5  (build-data-structure vec3 vec4))]
            (vector-append (vector start) (list->vector vec5) (vector end)))))
    
    (define (build-combined-vec vec1 vec2)
      (if (= 0 (vector-length vec1)) (make-vector 0)
            (vector-append (vector (vector (vector-ref vec1 0) (vector-ref vec2 0))) (build-combined-vec (vector-drop vec1 1) (vector-drop vec2 1)))))
    
    (define (build-data-structure vec4 vec5)
      (if (= 0 (vector-length vec4)) '()
          (append (list (mcons (vector-ref vec4 0) (vector-ref vec5 0))) (build-data-structure (vector-drop vec4 1) (vector-drop vec5 1)))))
    ))


  
  