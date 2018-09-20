#lang racket/base

(require racket/class
         surface/colors
         surface/node
         surface/vector)

(provide (all-defined-out))

(struct edge (node1 node2 c1 c2 c3 c4) #:mutable)

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
