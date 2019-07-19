#lang info

(define collection "surface")

(define deps '("base"
               "draw-lib"
               "gui-lib"
               "neuron-lib"
               "rackunit-lib"))

(define build-deps '("scribble-lib"))

(define scribblings '(("scribblings/surface.scrbl")))
