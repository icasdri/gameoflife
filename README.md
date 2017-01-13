# Conway's Game of Life

[Conway's Game of Life](https://en.wikipedia.org/wiki/Conway_game) implemented in [Rust](https://www.rust-lang.org/) with [piston](http://www.piston.rs/) (by default, graphics done by OpenGL).

## Controls

To pan the field of view:

* Click and drag anywhere in the window, *or*
* Scroll up, down, left, or right (with e.g. a scroll wheel)

To zoom:

* `Ctrl`+`+` to zoom in
* `Ctrl`+`-` to zoom out
* Alternatively, hold `Ctrl` while scrolling up and down

Other controls:

* Click directly on a cell to toggle alive/dead
* `Spacebar` to pause/resume generation ticks
* `[` and `]` to slow down and speed up (respectively) generation ticks

## Building

A recent [Rust](https://www.rust-lang.org/) toolchain is required for building. Piston will use the pure-Rust [glutin](https://github.com/tomaka/glutin) OpenGL context creation library by default (alleviating the need for linking against external libraries).

To build (a debug version):

    cargo build

To build then run directly:

    cargo run
