#lang racket/base

(require racket/class
         racket/draw
         surface/colors)

(provide (all-defined-out))

(struct node (bitmap position) #:mutable)

(define (draw-node dc n)
  (define pos (node-position n))
  (send dc draw-bitmap
        (node-bitmap n) (vector-ref pos 0) (vector-ref pos 1)))

(define (text-extent str)
  (send (new bitmap-dc% [bitmap (make-object bitmap% 1 1)])
        get-text-extent str))

(define (label str)
  (define-values (w h b c) (text-extent str))
  (define target (make-bitmap (inexact->exact (+ w 14))
                              (inexact->exact (+ h b))))
  (define dc (new bitmap-dc% [bitmap target]))
  ;; backing
  (send dc set-brush deep-gray 'solid)
  (send dc set-pen white 0 'transparent)
  (send dc draw-rounded-rectangle
        0 0 (inexact->exact (+ w 14)) (inexact->exact (+ h b)))
  ;; body
  (send dc set-text-mode 'transparent)
  (send dc set-text-foreground white)
  (send dc draw-text str 7 b)
  target)
