#lang racket/base

(require racket/class
         racket/draw)

(provide (all-defined-out))

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
