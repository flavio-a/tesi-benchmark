# Benchmark sources
This folder contains source codes for benchmarks. Each benchmark is in a different folder, and exports a module with the name of the benchmark. The module exports four functions:
- `bseq`, the sequential version (for comparison)
- `brepa`, the benchmark using Repa
- `bstrat`, the benchmark using strategies
- `bmpar`, the benchmark using monad Par

These functions return a data structure already evaluated as the problem requires, in order to avoid measurement errors related to lazy evaluation.
