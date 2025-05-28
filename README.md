### Klein
Klein is a small programming language based off of a compiler I created during
my time in CS 536 at the University of Wisconsin-Madison.  The original was written
in Java; I rewrote it in Rust to see how differently I'd have to write it to
replicate the original project.

#### Building
Klein can be built like any other Cargo project.  To compile a .kl file, run
```
cargo run -- file.kl
```
and the assembly will be output to `file.s`, which can be run with QtSpim.

`tests/alignment.kl` tests our ability to move bytes by copying part of the string,
"hello worble", to a lower portion of the address space.  Once loaded and run in QtSpim,
you should be able to see this in the 'Data' view.
