# Cellular Automata Playground

This is a sandbox for a exploring Conway's "Game of Life" and other cellular automata-based games. The rules of the game can easily be changed in `src/GameOfLife.elm` or a new game module exposing an `evolve : Grid -> Grid` function can be swapped.

Try it out [here](http://blitzrk.github.io/elm-ca-playground).

## Build & Test

``` bash
$ elm make Main.elm --output elm.js
$ python3 -m http.server
```

or

``` bash
$ ./devel.sh
```
