#lang racket/base

(require surface/edge
         surface/node
         surface/vector)

(provide (all-defined-out))

(define (repulsive-forces C K n nodes)
  (apply v+ #(0 0) (map (λ (m) (repulsive-force C K n m)) (remq n nodes))))

(define (repulsive-force C K n m)
  (define xi (node-position n))
  (define xj (node-position m))
  (v* (v- xj xi) (/ (* -1 C K K K) (vnorm-squared (v- xi xj)))))

(define (attractive-forces C K n edges)
  (define fa #(0 0))
  (define (fa+= m)
    (set! fa (v+ fa (attractive-force C K n m))))
  (for ([e edges])
    (cond
      [(eq? (edge-node1 e) n) (fa+= (edge-node2 e))]
      [(eq? (edge-node2 e) n) (fa+= (edge-node1 e))]))
  fa)

(define (attractive-force C K n m)
  (define xi (node-position n))
  (define xj (node-position m))
  (v* (v- xj xi) (/ (vnorm (v- xi xj)) K)))
