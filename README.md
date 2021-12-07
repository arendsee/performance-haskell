# performance-haskell

The following issues factor into high-performance Haskell code:

 * Efficient data structures (e.g., unboxed vectors)
 * Efficient algorithms that may use pure mutability (via the ST trick), fusion and such
 * Good memory, avoid space leaks
 * Parallelism (not concurrency)
 * Compiler optimizations such as inlining

And of course any work on performance needs a good method for benchmarking time and space.

This package is a sandbox for experimenting with all of these issues.
