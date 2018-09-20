#lang racket/base

(require fancy-app
         neuron
         racket/class
         racket/gui/base
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

    (define stepper (simulator (λ _ (send canvas refresh-now)) #:rate 60))

    (define/public (reset)
      (set! nodes null)
      (set! edges null))

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

    (define (update-nodes Δt)
      (for ([n nodes])
        (set-node-position!
         n (v+ (node-position n)
               (v* (v+ (repulsive-forces n nodes)
                       (attractive-forces n edges)) Δt)))))

    (define canvas (new canvas%
                        [parent parent]
                        [paint-callback surface-paint-callback]))))

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
         (set-edge-c4! e (random-ref '(#f #t))))))))
