#lang racket/gui
(require data/gvector)
(require "vertex.rkt")
(require "edge.rkt")
(require "Polygon1.rkt")
(require "monotone2.rkt")
(require "coloring.rkt")
(require "main.rkt")
;(require "rectangle.rkt")



(define first-page (new frame%
                        [label "The Art gallery problem"]
                        [min-width 100]
                        [min-height 100]
                        [stretchable-width #f]
                        [stretchable-height #f]))
(new button%
     [parent first-page]
     [label "Let's do it!"]
     ;[min-width 100]
     ;[min-width 50]
     (callback (lambda (button event)             
                 (send frame show #t)
                 (send first-page show #f)))
     )

(define (pics url-path)
  (read-bitmap (string->path url-path)))
(define image (pics "/home/naman/Desktop/Art-Gallery-Problem/973px-Triangulation_3-coloring.svg.png"))
(new message%
     [label image]
     [parent first-page])

(send first-page show #t)



(define frame (new frame% [label "Intro"][width 200][height 200]
                   [stretchable-width #f] [stretchable-height #f]))

(define my-canvas%
  (class canvas%
    (define/override (on-event event)
      (send msg set-label (string-append (number->string (send event get-x))"," (number->string (send event get-y)))))
    (define/override (on-char event)
      (define pressed (send event get-key-code))
      (cond ((char? pressed) (send msg set-label (string-append  "key pressed:" (make-string 1 pressed))))))
    ; Call the superclass init, passing on all init args
    (super-new)))

(define sides 0)

(new text-field% [parent frame]
     [label "Enter number of sides  "]
     ; Callback procedure for text-field:
     (callback (lambda (textbox event)
                 (set! sides (string->number (send textbox get-value)))))
     (style (list 'single  'horizontal-label))
     (min-width 10)
     (stretchable-width #f))

(define msg (new message% [parent frame]
                 [label ""]))


(define list-of-vec '())

(define method (new choice% [label "How would u like to make the polygon ?  "]
                    [choices (list "Plot it pictorially" "Give the coordinates")]
                    [parent frame]
                    (style (list 'vertical-label))))
(define p 0)
(new button% [parent frame]
     [label "Continue"]
     ; Callback procedure for a button click:
     (callback (lambda (button event)
                 (cond ((= (send method get-selection) 0)
                        (polygons sides))
                       ((= (send method get-selection) 1)
                        (coordinates sides)))
                 ))
     (vert-margin 15))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (show-coordinates dc list a)
  ;(let ((b ((if (= a 1) 0 900))))
  (define (helper counter listf)
    (cond ((not (null? listf))
           (send dc draw-text
                 (string-append (number->string counter) " ."
                                "(" (number->string (caar listf))
                                "," (number->string (cdar listf))
                                ")")
                 (caar listf) (- 900 (cdar listf)))
           (helper (+ counter 1) (cdr listf)))))
  (helper 1 (reverse list)))
(define listofver1 '())

(define (rotate-list lst)
  (append (cdr lst) (list (car lst))))
(define (process-input1 lst)             
  (define (helper lst optimal-pair)
    (if (equal? optimal-pair (car lst)) lst (helper (rotate-list lst) optimal-pair)))
  (let* [(optimal-pair  (foldr (lambda (x y) (cond [(> (cdr x) (cdr y)) x]
                                                   [(= (cdr x) (cdr y)) (if (< (car x) (car y)) x y)]
                                                   [#t y]))  (car lst) lst))]
    (helper lst optimal-pair)))
  
(define (polygons s)
 
  (define frame (new frame% [label "Drawing "]
                     [width 900]
                     [height 900]))
  
  (define drawing-listofver '())
  (define temporary-list '())
  (define listofver '())
  (define counter 1)

  (new message%
       [label "*please give points in clockwise manner"]
       [parent frame])
       
  (define slider (new slider%
                      [label "ease of making 90degree angle"]
                      [min-value 0]
                      [max-value 100]
                      [parent frame]))    
   


  (define (orthogonal dc vertices-list mouse-pos-vertex a)
    (define slope-of-prev-line
      (if (= (- (caar vertices-list) (caadr vertices-list)) 0)
          'infinty
          (/ (- (cdar vertices-list) (cdadr vertices-list))
             (- (caar vertices-list) (caadr vertices-list)))))
    (let* ((x1 (send mouse-pos-vertex get-x))
           (y1 (send mouse-pos-vertex get-y))
           (y2 (cdar vertices-list))
           (x2 (caar vertices-list))
           (slope-perp-line (cond ((eq? slope-of-prev-line 'infinty) 0)
                                  ((eq? slope-of-prev-line 0) 'infinity)
                                  (else (/ 1 (- slope-of-prev-line)))))
           (expected-y1 (cond ((and (not (eq? slope-perp-line 'infinity))
                                    (not (eq? slope-perp-line 0)))
                               (+ y2 (* slope-perp-line (- x1 x2)))))))
         
      (cond
        
        ((and (not (eq? slope-perp-line 'infinity))
              (not (eq? slope-perp-line 0))
              (or (< (abs (- x1 (+ x2 (/ (- y1 y2) slope-perp-line)))) a)
                  (< (abs (- y1 (+ y2 (* slope-perp-line (- x1 x2))))) a)))
         (begin (send dc draw-line x2 y2 x1 expected-y1)
                (set! temporary-list (cons x1 expected-y1))
                (send dc draw-text
                      (string-append "("
                                     (number->string x1)
                                     ","
                                     (number->string (- 900 expected-y1))
                                     ")")
                      x1 expected-y1)))


        ((and (eq? slope-perp-line 'infinity) (< (abs (- x1 x2)) a))
      
         (begin (send dc draw-line x2 y2 x2 y1)
                (set! temporary-list (cons x2 y1))
                (send dc draw-text
                      (string-append "("
                                     (number->string x2)
                                     ","
                                     (number->string (- 900 y1))
                                     ")")
                      x2 y1)))
      
        ((and (eq? slope-perp-line 0) (< (abs (- y1 y2)) a))
         (begin (send dc draw-line x2 y2 x1 y2)
                (set! temporary-list (cons x1 y2))
                (send dc draw-text
                      (string-append "("
                                     (number->string x1)
                                     ","
                                     (number->string (- 900 y2))
                                     ")")
                      x1 y2)))
               
    
        (else
         (begin (send dc draw-line x2 y2 x1 y1)
                (set! temporary-list (cons x1 y1))
                (send dc draw-text
                      (string-append "("
                                     (number->string x1)
                                     ","
                                     (number->string (- 900 y1))
                                     ")")
                      x1 y1))))))

  (define (get-final-vertex list-vertices dc mouse-vertex a)
    (let* ((slope-line1  (cond ((eq? (- (cdar (reverse list-vertices)) (cdadr (reverse list-vertices))) 0)
                                'infinty)
                               ((eq? (- (caar (reverse list-vertices)) (caadr (reverse list-vertices))) 0)
                                0)
                               (else (/ 1 (- (/ (- (cdar (reverse list-vertices)) (cdadr (reverse list-vertices)))
                                                (- (caar (reverse list-vertices)) (caadr (reverse list-vertices)))))))))
                       
           (slope-line2 (cond ((eq? (- (cdar list-vertices) (cdadr list-vertices)) 0)
                               'infinty)
                              ((eq? (- (caar list-vertices) (caadr list-vertices)) 0)
                               0)
                              (else (/ 1 (- (/ (- (cdar list-vertices) (cdadr list-vertices))
                                               (- (caar list-vertices) (caadr list-vertices))))))))
           (point1 (car (reverse list-vertices))) 
           (point2 (car list-vertices))
           (x  (cond  ((equal? slope-line1 slope-line2)
                      'no-x)
                     ((eq? slope-line1 'infinty)
                      (car point1))
                     ((eq? slope-line2 'infinty)
                      (car point2))
                     
                     (else (/ (+ (cdr point2) (- (cdr point1)) (* slope-line1 (car point1)) (- (* slope-line2 (car point2))))
                              (- slope-line1 slope-line2)))))
           (y (cond ((eq? x 'no-x)
                     'no-y)
                    ((eq? slope-line2 'infinty)
                     (cdr point2))
                    (else (+ (cdr point2) (- (* slope-line2 (car point2))) (* slope-line2 x)))))
           (mouse-x (send mouse-vertex get-x))
           (mouse-y (send mouse-vertex get-y)))

      (cond ((eq? x 'no-x)
             (begin (send dc draw-line (car point1) (cdr point1) mouse-x mouse-y)
                    (send dc draw-line (car point2) (cdr point2) mouse-x mouse-y)
                    (set! temporary-list (cons mouse-x mouse-y))
                    (send dc draw-text
                          (string-append "("
                                         (number->string mouse-x)
                                         ","
                                         (number->string (- 900 mouse-y))
                                         ")")
                          mouse-x mouse-y)))
           
            ((and (< (abs (- x mouse-x)) a ) (< (abs (- y mouse-y)) a ))
             (begin (send dc draw-line (car point1) (cdr point1) x y)
                    (send dc draw-line (car point2) (cdr point2) x y)
                    (set! temporary-list (cons x y))
                    (send dc draw-text
                          (string-append "("
                                         (number->string x)
                                         ","
                                         (number->string (- 900 y))
                                         ")")
                          x y)))
            (else (begin (send dc draw-line (car point1) (cdr point1) mouse-x mouse-y)
                         (send dc draw-line (car point2) (cdr point2) mouse-x mouse-y)
                         (set! temporary-list (cons mouse-x mouse-y))
                         (send dc draw-text
                               (string-append "("
                                              (number->string mouse-x)
                                              ","
                                              (number->string (- 900 mouse-y))
                                              ")")
                               mouse-x mouse-y))))))
  (define canvas1%
    (class canvas%
      (define/override (on-event event)
        (cond ((<= counter s)
               (if (send event button-down?)
                   (send canvas2 refresh-now 
                         (lambda (dc)
                           (if (= counter 1)
                               (begin (send draw-the-polygon move-to (send event get-x) (send event get-y))
                                      (set! listofver (cons (cons (send event get-x)
                                                                  (- 900 (send event get-y))) 
                                                            listofver))
                                      (set! drawing-listofver (cons (cons (send event get-x)
                                                                          (send event get-y))
                                                                    drawing-listofver))
                                      (set! counter (+ counter 1)))
                             
                               (begin (send draw-the-polygon line-to (car temporary-list)
                                            (cdr temporary-list))
                                      (send dc draw-path draw-the-polygon)
                                      (set! listofver (cons (cons (car temporary-list)
                                                                  (- 900 (cdr temporary-list))) 
                                                            listofver))
                                      (set! drawing-listofver (cons temporary-list
                                                                    drawing-listofver))
                           
                                      (show-coordinates dc listofver 0)
                                      (set! counter (+ counter 1)))) 
                         
                           (cond ((< s counter)
                                  
                                  (begin (send draw-the-polygon close)
                                         (send dc draw-path draw-the-polygon)
                                         (show-coordinates dc listofver 0)
                                         (set! listofver (process-input1 listofver))
                                         (set! listofver1 listofver))))))
                   (cond ((> counter 1)
                          (send canvas2 refresh-now 
                                (lambda (dc)
                                  (send dc set-pen "red" 2 'solid)
                                  (send dc draw-path draw-the-polygon)
                                  (show-coordinates dc listofver 0)
                                  (if (= counter 2)
                                      (begin (send dc draw-line
                                                   (caar drawing-listofver)
                                                   (cdar drawing-listofver)
                                                   (send event get-x)
                                                   (send event get-y))
                                             (set! temporary-list (cons (send event get-x)
                                                                        (send event get-y)))
                                             (send dc draw-text
                                                   (string-append "("
                                                                  (number->string
                                                                   (send event get-x))
                                                                  ","
                                                                  (number->string (- 900 (send event get-y)))
                                                                  ")")
                                                   (send event get-x) (send event get-y))) 
                                      (if (= counter s)
                                          (get-final-vertex drawing-listofver dc event (send slider get-value))
                                          (orthogonal dc drawing-listofver event
                                                      (send slider get-value))) 
                                      )))))))))
      (super-new)))       
  
  
  

  
  (define canvas2
    (new canvas1% [parent frame]
         [paint-callback
          (lambda (canvas2 dc) (paint dc))]
         ))


  (new button% [parent frame]
       [label "Finish"]
       (callback (lambda (button event)
                   (set! list-of-vec listofver1)
                   (set! p (lets-do-it))
                   (outputwindow p)))
                             
       (vert-margin 15))
  (new button% [parent frame]
       [label "Triangulation"]
       (callback (lambda (button event)
                   (set! list-of-vec listofver1)
                   (set! p (lets-do-it))
                   (outputwindow p)))
                             
       (vert-margin 15))


  (define draw-the-polygon  (new dc-path%))
  (define trace  (new dc-path%))



  (define (paint dc)
  
    (send dc set-pen "BLUE" 5 'solid)
    (send dc set-pen "red" 5 'solid)
    )

  (define x (make-object font% 50.0
              'default))
            
  (send frame show #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x_y (make-vector 2 #f))

(define (coordinates number)
  (define points (make-vector number))  
  
  (define frame2 (new frame% [label "coordinates"][width 200][height 200]
                      [stretchable-width #f] [stretchable-height #f]))
  (new message%
       [label "*please give points in Anticlockwise manner"]
       [parent frame2])

  (define rowpanel (new vertical-panel%
                        [parent frame2]
                        [alignment '(center top)]
                        [horiz-margin 50]
                        [vert-margin 10]
                        [spacing 20]))
  (define (coordinates-helper counter)
    (cond ((< counter number)
           (begin (define row1 (new horizontal-panel% 
                                    [parent rowpanel]
                                    [alignment '(left top)]))
                  (vector-set! points counter 
                               (vector
                                (new text-field% [parent row1]
                                     [label  (string-append "Point" (number->string (+ counter 1)) ".   " "x-cord  ")]
                                     [min-width 10])
                                (new text-field% [parent row1]
                                     [label "    y-cord   "]
                                     [min-width 10])))
                  (coordinates-helper (+ counter 1))))))
  (coordinates-helper 0)
  (define list-points '())
  (new button% [parent frame2]
       [label "Preview"]
       (callback (lambda (button event)
                   (begin (set! list-points (vector->list (vector-map
                                                           (lambda (vertex) (posn vertex))
                                                           points)))
                         
                          (preview-polygon list-points))))
       (vert-margin 15))
  
  (new button% [parent frame2]
       [label "Show me the position of cameras"]
       (callback (lambda (button event)
                   (begin (set! list-points (vector->list (vector-map
                                                           (lambda (vertex) (posn vertex))
                                                           points)))
                       
                          (set! listofver1 (process-input1 list-points))
                          (set! list-of-vec listofver1)
                          (set! p (lets-do-it))
                          (outputwindow p))))
       (vert-margin 10))
  
  (define (posn vertex)
    (cons (string->number (send (vector-ref vertex 0) get-value)) 
          (string->number(send (vector-ref vertex 1) get-value))))

  
  
  

  (define (preview-polygon points)
    (define frame3 (new frame% [label "Preview"][width 900][height 900]))
    
    


    (define canvas3
      (new canvas% [parent frame3]
           [paint-callback
            (lambda (canvas2 dc) (paint dc))]))
    
    
    (define (paint dc)
      (begin (send dc set-pen "red" 3 'solid)
             ;(send draw-the-polygon close)
             (send dc draw-polygon (map(lambda(vertex) (cons (car vertex)
                                                             (- 900 (cdr vertex))))
                                       list-points))
             (show-coordinates dc (reverse list-points) 0)
             ))
    (new button% [parent frame3]
         [label "Exit"]
         (callback (lambda (button event)
                     (send frame3 show #f)))
         (vert-margin 15))
    (send frame3 show #t))
  
  (send frame2 show #t))



(define monotone-vector '())
;(define p #f)
(define (lets-do-it)
  (define vec (list->vector (map (λ(x) (make-object vertex% (car x) (cdr x))) list-of-vec)))
  (define p (make-object polygon% (vector-length vec) vec))
  (send p break-to-monotone)
  ;(display (send p get-diagonals))
  (send p refresh-piecewise)
  (send p procedure)
  (send p procedure1)
  (define piece-wise-polygons (get-field piece-wise-polygons p))
  
  (set! monotone-vector
        (vector-map
         (λ(x)
           (make-object monotone% (vector-length x) x))
         piece-wise-polygons))
  
  (define complex-structures-from-monotone
    (vector-map
     (λ(x)
       (send x triangulate-it)) monotone-vector))
  (color-a-graph (gnodify complex-structures-from-monotone))
  p)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (get-cameras-vertices vector-vertices)
  (define (get-cameras-vertices-helper vector-list-of-vertices-of-same-color vertices)
    (if (null? vertices)
        (list->vector (max vector-list-of-vertices-of-same-color))  
        (begin (vector-set! vector-list-of-vertices-of-same-color
                            (- (send (car vertices) get-color-code) 1)
                            (cons (car vertices) (vector-ref
                                                  vector-list-of-vertices-of-same-color
                                                  (- (send (car vertices)
                                                           get-color-code) 1))))
               (get-cameras-vertices-helper
                vector-list-of-vertices-of-same-color
                (cdr vertices)))))

  (define (max vector-list-of-vertices-of-same-color)
    (define (max-return a b)
      (if (< (length a) (length b))
          a
          b))
    (define (max-helper counter recent-max)
      (if (= counter 2)
          (max-return
           (vector-ref vector-list-of-vertices-of-same-color counter)
           recent-max)
          
          (max-helper (+ counter 1) (max-return
                                     (vector-ref vector-list-of-vertices-of-same-color counter)
                                     recent-max))))
    (max-helper 0 (vector-ref vector-list-of-vertices-of-same-color 0)))
  
  (get-cameras-vertices-helper (make-vector 3 '()) (vector->list vector-vertices)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (outputwindow p)
  (define x (make-object font% 70.0
              'default))
  ;['style 'bold]))
  (define list-of-vertices (vector->list (send p get-vector-of-vertices)))
  (define list-of-camera-vertices
    (vector->list (get-cameras-vertices (send p get-vector-of-vertices))))
  (define list-of-posn (map (lambda (vertex)
                              (make-object point%
                                (get-field x-coor vertex)
                                (- 900 (get-field y-coor vertex))))
                            list-of-vertices))
  (define list-of-camera-posn (map (lambda (vertex)
                                     (make-object point%
                                       (get-field x-coor vertex)
                                       (- 900 (get-field y-coor vertex))))
                                   list-of-camera-vertices))

  
  
  (define frame (new frame% [label "Output Window"]
                     [width 900]
                     [height 900]))
  (define tab-panel (new tab-panel%
                         [choices (list
                                   "Polygon"
                                   "Monotone-polygons"
                                   "Triangulation"
                                   "Coloring"
                                   "Cameras position")]
                         [parent frame]
                         (callback (lambda (b e)
                                     (cond ((= (send b get-selection) 4)
                                            (send canvas2 refresh-now
                                                  (lambda (dc)
                                                    (send dc set-pen "black" 3 'solid)
                                                    (send dc draw-polygon list-of-posn)
                                                    (send dc set-pen (send  (car list-of-camera-vertices) color) 20 'solid)
                                                    (cameras-paint dc))))
                                           ((= (send b get-selection) 3)
                                            (send canvas2 refresh-now
                                                  (lambda (dc)
                                                    (send dc set-pen "black" 3 'solid)
                                                    (send dc draw-polygon list-of-posn)
                                                    (make-triangles dc)
                                                    (coloring-paint dc))))
                                           ((= (send b get-selection) 2)
                                            (send canvas2 refresh-now
                                                  (lambda (dc)
                                                    (send dc set-pen "black" 3 'solid)
                                                    (send dc draw-polygon list-of-posn)
                                                    (send dc set-pen "Black" 3 'solid)
                                                    (make-triangles dc)
                                                    )))
                                           ((= (send b get-selection) 1)
                                            (send canvas2 refresh-now
                                                  (lambda (dc)
                                                    (send dc set-pen "black" 3 'solid)
                                                    (send dc draw-polygon list-of-posn)
                                                    (send dc set-pen "black" 5 'solid)
                                                    (make-monotone dc))))
                                           ((= (send b get-selection) 0)
                                            (send canvas2 refresh-now
                                                  (lambda (dc)
                                                    (send dc set-pen "red" 3 'solid)
                                                    (send dc draw-polygon list-of-posn)
                                                    (show-coordinates dc listofver1 0)))))))
                         (font x)))
                                          
                                         
  (define canvas2
    (new canvas% [parent tab-panel]
         [paint-callback
          (lambda (canvas2 dc) (paint dc))]
         ))

  (define vec-nodes (vector-map (lambda (x) (mcar x)) final-structure))
  (define list-nodes (vector->list vec-nodes))

  (define  (make-triangles dc)
    (define (helper list)
      (cond ((not (null? list))
             (send dc draw-polygon (make-list-of-points-of-current-node (car list)))
             (helper (cdr list)))))
    (helper list-nodes))

  (define (make-list-of-points-of-current-node node)
    (let ((list-vertices (vector->list (get-field vec node))))
      (map (lambda (vertex) (cons (get-field x-coor vertex) (- 900 (get-field y-coor vertex))))
           list-vertices)))


  (define (make-monotone dc)
    (let ((diagonals-vec (send p get-diagonals)))
      (for [(counter(vector->list diagonals-vec))]
        (send dc draw-line (get-field x-first counter)
              (- 900 (get-field y-first counter))
              (get-field x-second  counter)
              (- 900 (get-field y-second counter))))))
        
  



  (define (paint dc)
  
    (send dc set-pen "red" 3 'solid)
    (send dc draw-polygon list-of-posn)
    (show-coordinates dc listofver1 0))
                        
  (define (cameras-paint dc)

    (define (helper list)
      (cond ((not (null? list))
             (begin (send dc draw-point (send (car list) get-x) (send (car list) get-y))
                      
                    (helper (cdr list))))))
    (helper list-of-camera-posn))
  (define (coloring-paint dc)

    (define (helper list)
      (cond ((not (null? list))
             
             (begin
               (send dc set-pen (send (car list) color) 20 'solid)
               (send dc draw-point (get-field x-coor (car list)) (- 900 (get-field y-coor (car list) )))
               (helper (cdr list))))))
    (helper list-of-vertices))
            

  
            
  (send frame show #t))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

