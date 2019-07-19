#lang racket/base

(require neuron
         racket/class
         racket/flonum
         racket/format
         racket/gui/base
         racket/math
         racket/random
         surface/colors
         surface/edge
         surface/forces
         surface/node
         surface/vector)

(provide (all-defined-out))

(define surface%
  (class object%
    (init parent)
    (super-new)

    (define t0 (current-inexact-milliseconds))
    (define paused? #f)

    (define/public (pause)
      (set! paused? #t))

    (define/public (unpause)
      (set! paused? #f))

    (define named-nodes (make-hasheq))
    (define origin #(0 0))
    (define nodes null)
    (define edges null)
    (define C 0.2)
    (define K 10)

    (define/public (reset)
      (define-values (w h) (send canvas get-client-size))
      (hash-clear! named-nodes)
      (set! origin (vector (/ w 2) (/ h 2)))
      (set! nodes null)
      (set! edges null)
      (set! K 10))

    (define/public (get-state)
      (list (hash->list named-nodes) origin nodes edges C K))

    (define/public (set-state state)
      (set! named-nodes (make-hasheq (car state)))
      (set! origin (cadr state))
      (set! nodes (caddr state))
      (set! edges (cadddr state))
      (set! C (car (cddddr state)))
      (set! K (cadr (cddddr state))))

    (define/public (zoom factor)
      (set! K (* factor 10)))

    (define/public (pan Δx Δy)
      (set! origin (v+ origin (vector Δx Δy))))

    (define/public (add-node name bmp [x #f] [y #f])
      (define w (send bmp get-width))
      (define h (send bmp get-height))
      (define pos
        (v- (vector (or x (random -50 50))
                    (or y (random -50 50)))
            (vector (/ w 2) (/ h 2))))
      (define n (node bmp pos))
      (set! nodes (cons n nodes))
      (set! K (+ K 1))
      (when name (hash-set! named-nodes name n))
      n)

    (define/public (get-node name)
      (hash-ref named-nodes name #f))

    (define/public (add-edge n m)
      (unless (node? n) (set! n (hash-ref named-nodes n)))
      (unless (node? m) (set! m (hash-ref named-nodes m)))
      (or (get-edge n m)
          (let ([e (edge n m #f #f #f #f)])
            (set! edges (cons e edges))
            e)))

    (define/public (set-edge n m c1 c2 c3 c4)
      (define e (add-edge n m))
      (set-edge-c1! e c1)
      (set-edge-c2! e c2)
      (set-edge-c3! e c3)
      (set-edge-c4! e c4))

    (define (get-edge n m)
      (unless (node? n) (set! n (hash-ref named-nodes n)))
      (unless (node? m) (set! m (hash-ref named-nodes m)))
      (findf (λ (e)
               (or (and (eq? n (edge-node1 e)) (eq? m (edge-node2 e)))
                   (and (eq? m (edge-node1 e)) (eq? n (edge-node2 e)))))
             edges))

    (define stepper
      (simulator (λ _ (send canvas refresh-now)) #:rate 30))

    (define (surface-paint-callback canvas dc)
      ;; update node positions
      (define t (current-inexact-milliseconds))
      (unless paused? (update-nodes (/ (- t t0) 1000)))
      (set! t0 t)
      ;; clean slate
      (send dc set-background deep-gray)
      (send dc clear)
      ;; draw coordinate grid
      (define x0 (vector-ref origin 0))
      (define y0 (vector-ref origin 1))
      (send dc set-pen dark-gray 0.5 'solid)
      (send dc draw-line x0 y0 (+ x0 2.5) (- y0 2.5))
      (send dc draw-line x0 y0 (+ x0 2.5) (+ y0 2.5))
      (send dc draw-line x0 y0 (- x0 2.5) (+ y0 2.5))
      (send dc draw-line x0 y0 (- x0 2.5) (- y0 2.5))
      (define-values (w h) (send dc get-size))
      (define dx (remainder (exact-round x0) 10))
      (define dy (remainder (exact-round y0) 10))
      (for ([i (in-range 0 w 10)])
        (send dc draw-line (+ i dx) 0 (+ i dx) h))
      (for ([j (in-range 0 h 10)])
        (send dc draw-line 0 (+ j dy) w (+ j dy)))
      ;; redraw nodes and edges
      (for ([e edges]) (draw-edge dc origin e))
      (for ([n nodes]) (draw-node dc origin n)))

    (define (update-nodes Δt)
      (for ([n nodes])
        (set-node-position!
         n (v+ (node-position n)
               (v* (v+ (repulsive-forces C K n nodes)
                       (attractive-forces C K n edges)) Δt)))))

    (define the-surface this)

    (define canvas
      (new
       (class canvas%
         (define/override (on-char event)
           (case (send event get-key-code)
             [(#\space) (if paused? (unpause) (pause))]
             [(#\rubout) (unpause) (reset)]
             [(#\r) (unpause) (random-network the-surface 15)]
             [(#\-) (set! K (- K 5))]
             [(#\=) (set! K (+ K 5))]
             [(left ) (pan  10   0)]
             [(right) (pan -10   0)]
             [(up   ) (pan   0  10)]
             [(down ) (pan   0 -10)]
             [(#\Q) (exit)]
             [else (let ([key (send event get-key-code)])
                     (unless (eq? key 'release) (writeln `(key ,key))))]))

         (define/override (on-size new-w new-h)
           (set! origin (vector (/ new-w 2) (/ new-h 2))))

         (super-new))
       [parent parent]
       [paint-callback surface-paint-callback]))

    (send canvas focus)))

(define (random-network surface size)
  (send surface reset)
  (define ns
    (for/list ([i size])
      (sleep (/ (random) 20))
      (send surface add-node i (label (~a i)))))
  (define es
    (append
     (for/list ([n ns])
       (sleep (/ (random) 20))
       (send surface add-edge n (random-ref (remq n ns))))
     (for/list ([_ (random size)])
       (sleep (/ (random) 20))
       (define n+m (random-sample ns 2))
       (send surface add-edge (car n+m) (cadr n+m)))))
  (simulator
   #:rate 6
   (λ _
     (for ([e es])
       (set-edge-c1! e (random-ref '(#f #t)))
       (set-edge-c2! e (random-ref '(#f #t)))
       (set-edge-c3! e (random-ref '(#f #t)))
       (set-edge-c4! e (random-ref '(#f #t)))))))

(module+ main
  (require rackunit)

  (define frame (new frame% [label "surface-frame"]))
  (define surface (new surface% [parent frame]))
  (send frame show #t))
