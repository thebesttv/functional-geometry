;;; cairo

(use-modules (cairo))

(define IMAGE-WIDTH 800)
(define IMAGE-HEIGHT 800)

(define surface '())
(define cr '())

;;; vector

(define (make-vec x y) (cons x y))
(define (x-vec v) (car v))
(define (y-vec v) (cdr v))

(define (print-vec v)
  (display "[")
  (display (x-vec v))
  (display ",")
  (display (y-vec v))
  (display "]"))

(define (+vec a b)
  (make-vec (+ (x-vec a) (x-vec b))
            (+ (y-vec a) (y-vec b))))

(define (-vec a b)
  (make-vec (- (x-vec a) (x-vec b))
            (- (y-vec a) (y-vec b))))

(define (scale-vec s v)
  (make-vec (* s (x-vec v))
            (* s (y-vec v))))

;;; segment

(define (make-seg a b) (cons a b))
(define (start-seg s) (car s))
(define (end-seg s) (cdr s))

;;; frame

(define (make-frame origin horiz vert)
  (cons origin
        (cons horiz vert)))

(define (origin-frame frame) (car frame))
(define (horiz-frame frame) (cadr frame))
(define (vert-frame frame) (cddr frame))

(define (frame-coord-map frame)
  (lambda (vec)
    (+vec (origin-frame frame)
          (+vec (scale-vec (x-vec vec) (horiz-frame frame))
                (scale-vec (y-vec vec) (vert-frame frame))))))

;; ((frame-coord-map (make-frame (make-vec 2 2)
;;                               (make-vec 4 0)
;;                               (make-vec 0 8)))
;;  (make-vec 0.5 0.5))


;;; painter --- A painter is a procedure that, given a frame as
;;; argument, draw a particular image shifted & scaled to fit that
;;; frame.  Any procedure can serve as a painter, provided that it
;;; takes a frame as argument and draws something scaled to fit the
;;; frame.

(define (transform-matrix frame)
  (define (scale s . l)
    (map (lambda (x) (* s x)) l))
  (let  ([origin (origin-frame frame)]
         [horiz (horiz-frame frame)]
         [vert (vert-frame frame)])
    (list->typed-array
     'f64 2
     (list (scale IMAGE-WIDTH (x-vec horiz) (x-vec vert) (x-vec origin))
           (scale IMAGE-HEIGHT (- (y-vec horiz)) (- (y-vec vert)) (- 1 (y-vec origin)))
           (list 0 0 1)))))

(define (segments->painter segment-list)
  (define (draw-line a b)
    (cairo-set-source-rgba cr 0 0 0 1)
    (cairo-move-to cr (x-vec a) (y-vec a))
    (cairo-line-to cr (x-vec b) (y-vec b))
    (cairo-set-line-width cr 0.01)
    (cairo-stroke cr))
  (lambda (frame)
    (cairo-set-matrix cr (transform-matrix frame))
    (for-each (lambda (s)
                (draw-line (start-seg s)
                           (end-seg s)))
              segment-list)))

;;; painter transformation

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let* ([m (frame-coord-map frame)]
           [new-origin (m origin)])
      (painter (make-frame new-origin
                           (-vec (m corner1) new-origin)
                           (-vec (m corner2) new-origin))))))

(define (flip-vert painter)
  ;; upside-down
  (transform-painter painter
                     (make-vec 0.0 1.0)
                     (make-vec 1.0 1.0)
                     (make-vec 0.0 0.0)))

(define (flip-horiz painter)
  ;; left-to-right reversed
  (transform-painter painter
                     (make-vec 1.0 0.0)
                     (make-vec 0.0 0.0)
                     (make-vec 1.0 1.0)))

(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vec 0.5 0.5)
                     (make-vec 1.0 0.5)
                     (make-vec 0.5 1.0)))

(define (rotate90 painter)
  ;; rotate counterclockwise by 90 degress
  (transform-painter painter
                     (make-vec 1.0 0.0)
                     (make-vec 1.0 1.0)
                     (make-vec 0.0 0.0)))

(define (rotate180 painter)
  (rotate90 (rotate90 painter)))

(define (rotate270 painter)
  (rotate90 (rotate90 (rotate90 painter))))

(define (squash-inwards painter)
  ;; squash towards the center of the frame
  (transform-painter painter
                     (make-vec 0.0 0.0)
                     (make-vec 0.65 0.35)
                     (make-vec 0.35 0.65)))

(define (beside left right)
  (let* ([split 0.5]
         [split-point (make-vec split 0.0)]
         [paint-left (transform-painter left
                                        (make-vec 0.0 0.0)
                                        split-point
                                        (make-vec 0.0 1.0))]
         [paint-right (transform-painter right
                                         split-point
                                         (make-vec 1.0 0.0)
                                         (make-vec split 1.0))])
    (lambda (frame)
      (paint-left frame)
      (paint-right frame))))

;; ((beside (segments->painter (append x-segments))
;;          (segments->painter (append x-segments)))
;;  unit-frame)

(define (below down up)
  (let* ([split 0.5]
         [split-point (make-vec 0.0 split)]
         [paint-down (transform-painter down
                                        (make-vec 0.0 0.0)
                                        (make-vec 1.0 0.0)
                                        split-point)]
         [paint-up (transform-painter up
                                      split-point
                                      (make-vec 1.0 split)
                                      (make-vec 0.0 1.0))])
    (lambda (frame)
      (paint-down frame)
      (paint-up frame))))

;;;

(define (flipped-pairs painter)
  (let ([pair (beside painter (flip-vert painter))])
    (below painter painter)))

(define (right-split painter n)
  (if (zero? n)
      painter
      (let ([smaller (right-split painter (- n 1))])
        (beside painter
                (below smaller smaller)))))

(define (up-split painter n)
  (if (zero? n)
      painter
      (let ([smaller (up-split painter (- n 1))])
        (below painter
               (beside smaller smaller)))))

(define (corner-split painter n)
  (if (zero? n)
      painter
      (let* ([up (up-split painter (- n 1))]
             [right (right-split painter (- n 1))]
             [top-left (beside up up)]
             [bottom-right (below right right)]
             [top-right (corner-split painter (- n 1))])
        (beside (below painter top-left)
                (below bottom-right top-right)))))

(define (square-limit painter n)
  (let* ([quarter (corner-split painter n)]
         [half (beside (flip-horiz quarter) quarter)])
    (below (flip-vert half) half)))

;;;

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ([top    (beside (tl painter) (tr painter))]
          [bottom (beside (bl painter) (br painter))])
      (below bottom top))))

(define (flipped-pairs painter)
  ((square-of-four identity flip-vert
                   identity flip-vert)
   painter))

(define flipped-pairs
  (square-of-four identity flip-vert
                  identity flip-vert))

(define (square-limit painter n)
  (let ([combine4 (square-of-four flip-horiz identity
                                  rotate180  flip-vert)])
    (combine4 (corner-split painter n))))

(define (split f g)
  (define (fun painter n)
    (if (zero? n)
        painter
        (let ([smaller (fun painter (- n 1))])
          (f painter (g smaller smaller)))))
  fun)

(define right-split (split beside below))
(define up-split    (split below beside))

;;; main

(define border-segments
  (list (make-seg (make-vec 0 0) (make-vec 0 1))
        (make-seg (make-vec 0 0) (make-vec 1 0))
        (make-seg (make-vec 1 1) (make-vec 0 1))
        (make-seg (make-vec 1 1) (make-vec 1 0))))

(define x-segments
  (list (make-seg (make-vec 0 0) (make-vec 1 1))
        (make-seg (make-vec 0 1) (make-vec 1 0))))

(define unit-frame (make-frame (make-vec 0.0 0.0)
                               (make-vec 1.0 0.0)
                               (make-vec 0.0 1.0)))

(define (z-painter frame)
  (cairo-set-matrix cr (transform-matrix frame))
  (let ([x  .1] [y  (- 1 .5)]
        [x1 .4] [y1 (- 1 .9)]
        [x2 .6] [y2 (- 1 .1)]
        [x3 .9] [y3 (- 1 .5)])
    (cairo-set-line-width cr 0.04)
    (cairo-set-source-rgba cr 0 0 0 1)
    (cairo-move-to cr x y)
    (cairo-curve-to cr x1 y1 x2 y2 x3 y3)
    (cairo-stroke cr)
    (cairo-set-source-rgba cr 1 .2 .2 .6)
    (cairo-set-line-width cr 0.03)
    (cairo-move-to cr x y)
    (cairo-line-to cr x1 y1)
    (cairo-move-to cr x2 y2)
    (cairo-line-to cr x3 y3)
    (cairo-stroke cr)))

(define tree-painter (segments->painter
                      (list (make-seg '(.5 . .9) '(.3 . .7))
                            (make-seg '(.5 . .9) '(.7 . .7))
                            (make-seg '(.3 . .7) '(.7 . .7))
                            (make-seg '(.5 . .7) '(.1 . .1))
                            (make-seg '(.5 . .7) '(.9 . .1))
                            (make-seg '(.1 . .1) '(.9 . .1)))))


(let ([file-name  "/tmp/ex1.png"]
      [painter tree-painter])

  (set! surface (cairo-image-surface-create 'argb32 IMAGE-WIDTH IMAGE-HEIGHT))
  (set! cr (cairo-create surface))

  (cairo-set-source-rgba cr 1 1 1 1)
  (cairo-paint cr)
  ;; (let ([small (beside painter painter)])
  ;;   ((below small small)
  ;;    unit-frame))

  ((corner-split painter 5) unit-frame)
  ;; ((square-limit painter 4) unit-frame)
  ;; ((right-split painter 4) unit-frame)

  ;; (painter unit-frame)

  (cairo-stroke cr)
  (cairo-surface-write-to-png surface file-name)
  (cairo-destroy cr)
  (cairo-surface-destroy surface))
