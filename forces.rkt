#lang racket/base

(require surface/edge
         surface/node
         surface/vector)

(provide (all-defined-out))

(define C 0.2)
(define K 30)

(define (repulsive-forces n nodes)
  (apply v+ #(0 0) (map (λ (m) (repulsive-force n m)) (remq n nodes))))

(define (repulsive-force n m)
  (define xi (node-position n))
  (define xj (node-position m))
  (v* (v- xj xi) (/ (* -1 C K K K) (vnorm-squared (v- xi xj)))))

(define (attractive-forces n edges)
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
