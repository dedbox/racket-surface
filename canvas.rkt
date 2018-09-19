#lang racket/base

(require fancy-app
         neuron
         racket/class
         racket/gui/base
         (only-in racket/list flatten)
         racket/math
         racket/pretty
         racket/random
         racket/vector)

(provide (all-defined-out))

(define C 0.2)
(define K 100)

(define white (make-object color% #xff #xff #xff))
(define deep-gray (make-object color% #x26 #x32 #x38))
(define dark-gray (make-object color% #x45 #x5A #x64))
(define light-gray (make-object color% #x78 #x90 #x9c))
(define red (make-object color% #xf3 #x6c #x60))
(define orange (make-object color% #xf9 #x80 #x0))
(define yellow (make-object color% #xff #xf5 #x9d))
;; (define green (make-object color% #x8b #xc3 #x4a))
(define aqua (make-object color% #x81 #xd4 #xfa))
(define blue (make-object color% #x4d #xd0 #xe1))
(define purple (make-object color% #xb3 #x9d #xdb))
(define green (make-object color% #x9c #xcc #x65))

(define (label str)
  (define-values (w h b v)
    (send (new bitmap-dc% [bitmap (make-object bitmap% 1 1)])
          get-text-extent str))
  (define target (make-bitmap (inexact->exact (+ w 14))
                              (inexact->exact (+ h b))))
  (define dc (new bitmap-dc% [bitmap target]))
  (send dc set-brush deep-gray 'solid)
  (send dc set-pen white 0 'transparent)
  (send dc draw-rounded-rectangle
        0 0 (inexact->exact (+ w 14)) (inexact->exact (+ h b)))
  (send dc set-text-mode 'transparent)
  (send dc set-text-foreground white)
  (send dc draw-text str 7 b)
  target)

(struct node (bitmap position) #:mutable)
(struct edge (node1 node2 c1 c2 c3 c4) #:mutable)

(define (v+ . vecs)
  (apply vector-map + vecs))

(define (v- . vecs)
  (apply vector-map - vecs))

(define (v* vec val)
  (vector-map (* _ val) vec))

(define (v/ vec val)
  (vector-map (/ _ val) vec))

(define (vnorm-squared vec)
  (for/sum ([v vec]) (* v v)))

(define (vnorm vec)
  (sqrt (vnorm-squared vec)))

(define (voffset vec1 vec2 amount)
  (define x1 (vector-ref vec1 0))
  (define y1 (vector-ref vec1 1))
  (define x2 (vector-ref vec2 0))
  (define y2 (vector-ref vec2 1))
  (define θ1 (atan (/ (- y2 y1) (- x2 x1))))
  (define θ2 (- (/ pi 2) θ1))
  (values (vector (- x1 (* amount (cos θ2)))
                  (+ y1 (* amount (sin θ2))))
          (vector (- x2 (* amount (cos θ2)))
                  (+ y2 (* amount (sin θ2))))))

(define surface%
  (class object%
    (init parent)
    (super-new)

    (define nodes null)
    (define edges null)

    (define/public (add-node bmp)
      (define-values (w h) (send canvas get-client-size))
      (define pos (v+ (v/ (vector w h) 2)
                      (vector (random -50 50)
                              (random -50 50))))
      (define n (node bmp pos))
      (set! nodes (cons n nodes))
      n)

    (define/public (add-edge n m)
      (or (get-edge n m)
          (let ([e (edge n m #f #f #f #f)])
            (set! edges (cons e edges))
            e)))

    (define (get-edge n m)
      (findf (λ (e)
               (or (and (eq? n (edge-node1 e)) (eq? m (edge-node2 e)))
                   (and (eq? m (edge-node1 e)) (eq? n (edge-node2 e)))))
             edges))

    (define/public (reset)
      (set! nodes null)
      (set! edges null))

    (define (update-nodes Δt)
      (for ([n nodes])
        (set-node-position! n (v+ (node-position n)
                                  (v* (repulsive-forces n) Δt)
                                  (v* (attractive-forces n) Δt)))))

    (define (repulsive-forces n)
      (apply v+ #(0 0) (map (λ (m) (repulsive-force n m)) (remq n nodes))))

    (define (repulsive-force n m)
      (define xi (node-position n))
      (define xj (node-position m))
      (v* (v- xj xi) (/ (* -1 C K K) (vnorm-squared (v- xi xj)))))

    (define (attractive-forces n)
      (define fa #(0 0))
      (define (fa+= m)
        (set! fa (v+ fa (attractive-force n m))))
      (for ([e edges])
        (cond
          [(eq? (edge-node1 e) n) (fa+= (edge-node2 e))]
          [(eq? (edge-node2 e) n) (fa+= (edge-node1 e))]))
      fa)

    (define (attractive-force n m)
      (define xi (node-position n))
      (define xj (node-position m))
      (v* (v- xj xi) (/ (vnorm (v- xi xj)) K)))

    (define t0 (current-inexact-milliseconds))

    (define (surface-paint-callback canvas dc)
      ;; update node positions
      (define t (current-inexact-milliseconds))
      (update-nodes (/ (- t t0) 1000))
      (set! t0 t)
      ;; redraw nodes and edges
      (send dc set-background deep-gray)
      (send dc clear)
      (for ([e edges]) (draw-edge dc e))
      (for ([n nodes]) (draw-node dc n)))

    (define (draw-edge dc e)
      (define p1 (v+ (node-position (edge-node1 e)) #(10 10)))
      (define p2 (v+ (node-position (edge-node2 e)) #(10 10)))
      (send dc set-pen dark-gray 7 'solid)
      (send dc draw-line
            (vector-ref p1 0) (vector-ref p1 1)
            (vector-ref p2 0) (vector-ref p2 1))
      (define-values (p1- p2-) (voffset p1 p2 -3.5))
      (define-values (p1+ p2+) (voffset p1 p2  3.5))
      (define p1/2- (v+ p1- (v/ (v- p2- p1-) 2)))
      (define p1/2+ (v+ p1+ (v/ (v- p2+ p1+) 2)))
      (when (edge-c1 e) (draw-edge-corner dc red    p1-   p1/2-))
      (when (edge-c2 e) (draw-edge-corner dc green  p1/2- p2-  ))
      (when (edge-c3 e) (draw-edge-corner dc purple p1+   p1/2+))
      (when (edge-c4 e) (draw-edge-corner dc blue   p1/2+ p2+  )))

    (define (draw-edge-corner dc color p1 p2)
      (send dc set-pen color 3 'solid)
      (send dc draw-line
            (vector-ref p1 0) (vector-ref p1 1)
            (vector-ref p2 0) (vector-ref p2 1)))

    (define (draw-node dc n)
      (define pos (node-position n))
      (send dc draw-bitmap
            (node-bitmap n) (vector-ref pos 0) (vector-ref pos 1)))

    (define canvas (new canvas%
                        [parent parent]
                        [paint-callback surface-paint-callback]))

    (define stepper (simulator (λ _ (send canvas refresh-now)) #:rate 60))))

(module+ test
  (require racket/format
           rackunit)

  (define frame (new frame% [label "surface-frame"]))
  (define surface (new surface% [parent frame]))
  (send frame show #t)

  (define (random-network size)
    (send surface reset)
    (define ns
      (for/list ([i size])
        (sleep (/ (random) 20))
        (send surface add-node (label (~a i)))))
    (for ([n ns])
      (sleep (/ (random) 20))
      (define m n)
      (let loop () (when (eq? m n) (set! m (random-ref ns)) (loop)))
      (define e (send surface add-edge n m))
      (set-edge-c1! e (random-ref '(#f #t)))
      (set-edge-c2! e (random-ref '(#f #t)))
      (set-edge-c3! e (random-ref '(#f #t)))
      (set-edge-c4! e (random-ref '(#f #t))))
    (for ([_ (random size)])
      (sleep (/ (random) 20))
      (define n+m (random-sample ns 2))
      (send surface add-edge (car n+m) (cadr n+m)))))
