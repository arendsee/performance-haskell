# performance-haskell

The following issues factor into high-performance Haskell code:

 * Efficient data structures (e.g., vectors rather than lists and HAMTs (Hashed Array Mapped Tries))
 * Efficient algorithms that use mutability as needed (pure, ideally) 
 * Good memory, avoid space leaks, lazy IO for streaming large files in constant memory
 * Parallelism (not concurrency)
 * Compiler optimizations such as inlining and steam fusion
 * Distributed computing - see FPComplete's https://www.fpcomplete.com/assets/SCALING-UP-A-SCIENTIFIC-COMPUTATION.pdf
 * Database access

And of course any work on performance needs a good method for benchmarking time and space.

This package is a sandbox for experimenting with all of these issues.
