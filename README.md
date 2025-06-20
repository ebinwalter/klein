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

Klein can also be built into a Docker image, by running `docker build -t klein .` and then `docker run -t klein`, or simply `docker-compose up --build` if you have installed `docker-compose`.  The default entrypoint for the image runs the unit tests.

### Development
If you are extending Klein, you may find it helpful to write unit tests.  A small primitive shell script, `tests/run_tests.sh`, is designed to help you do so.
To check the output of all compiled .kl files against expected outputs, you can simply run `./tests/run_tests.sh`.  To generate outputs to check future runs against, run `makerefs=1 ./tests/run_tests.sh`.  When making references, it's always a good idea to check that things were still running properly when you generated the tests by taking a peek at the text files within tests/expected.

If the unit test you're writing doesn't yet compile because it contains a syntactic construct not yet handled by the codebase, you can manually write what you expect to see as the output once other expected results have been generated.  **Be mindful not to accidentally overwrite your expected output**; otherwise, **failing tests will appear to pass** because the run is being tested against output which indicates a failure.

### Documentation
There's no documentation for Klein as of yet.  Honestly, documentation might only be provided inasmuch as it helps me remember details of my code or test it, because I want to move on to a more interesting language whenever I feel like I've exhausted myself on this one.  Hopefully, you can glean the syntax from the `tests` folder. 

And at the very least, the function pointer syntax is designed to be more intuitive than C's.  Since no more than a handful of people will ever use this language at a time, just reach out if you have a question, and I (Evan) should be able to answer it.
