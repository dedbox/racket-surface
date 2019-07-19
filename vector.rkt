#lang racket/base

(require racket/math
         racket/vector)

(provide (all-defined-out))

(define (v+ . vecs)
  (apply vector-map + vecs))

(define (v- . vecs)
  (apply vector-map - vecs))

(define (v* vec . vs)
  (vector-map (λ (v) (apply * v vs)) vec))

(define (v/ vec . vs)
  (vector-map (λ (v) (apply / v vs)) vec))

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
