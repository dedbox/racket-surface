# A 2-D Network Visualization Surface (prototype)

# Installation

From the command line:
```
$ raco pkg install --clone ./surface https://github.com/dedbox/racket-surface.git
```

# Usage

Generates a random network of nodes and decorated edges inside a spring-force
simulator for automatic layout.

Right now, all the action is in `canvas.rkt`. Its `main` sub-module starts the
demo. To start the simulator from the command line, run:
```
$ racket ./surface/canvas.rkt
```

Key bindinds:
```
 <spacebar>                   | pause / unpause
 <backspace>                  | reset
 r                            | randomize
 = / -                        | zoom in / out
 <left> / <right> / up / down | pan
 Q                            | quit
```

This repository is for educational and historical reference only. For future
developments, see https://github.com/dedbox/racket-viz.
