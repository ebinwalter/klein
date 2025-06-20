## Klein
Klein is a compiler pet-project for a C-like programming language targeting MIPS32 assembly.  The skeleton of the design is based on Bach, a much more minimal C-like language developed in CS 536 at the University of Wisconsin-Madison.  My improvements over what we implemented before the semester ended include:

- codegen for structs
- var decls anywhere within a code block
- pointers
- arrays
- register allocation
- string input
- character literals
- function pointers
- better syntax, in my opinion

### Usage
Klein can be compiled very simply with `cargo build`, having no dependencies not managed by Cargo already.  
The name of the executable is `kleinc`, and it takes a single source file as an argument.

Since no Rust crate exists for simulating MIPS in the capacity I need it, the Docker image is built with a copy of MARS 4.5 for running the output of the compiler.

Tests can be run outside of a Docker context provided you have a Java installation by running `tests/run_tests.sh`, but the Dockerfile is written to run with minimal effort on your part.

### Development
If you are extending Klein, you may find it helpful to write unit tests.  A small primitive shell script, `tests/run_tests.sh`, is designed to help you do so.
To check the output of all compiled .kl files against expected outputs, you can simply run `./tests/run_tests.sh`.  To generate outputs to check future runs against, run `makerefs=1 ./tests/run_tests.sh`.

If the unit test you're writing doesn't yet compile because it contains a syntactic construct not yet handled by the codebase, you can manually write what you expect to see as the output once other expected results have been generated.  **Be mindful not to accidentally overwrite your expected output**; otherwise, **failing tests will appear to pass** because the run is being tested against output which indicates a failure.
