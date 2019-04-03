# Benchmark sources
This folder contains source codes for benchmarks. Each benchmark is in a different folder, and exports a module with the name of the benchmark. The module exports at least four functions:
- `bseq`, the sequential version (for comparison)
- `brepa`, the benchmark using Repa
- `bstrat`, the benchmark using strategies
- `bmpar`, the benchmark using monad Par

These functions return a data structure already evaluated as the problem requires, in order to avoid measurement errors related to lazy evaluation.

`brepa` may not have the same signature as other benchmark functions. If this is the case the module also exports a `brepatest` function, only useful for testing, that has the right signature.

Each module may export any type or utility function that may be useful in testing or benchmarking (eg: types used in function signatures).

# General notes
## Thresholds
To achieve better results parallelism almost always has a threshold, that is at some point in the computation the parallel program switches to sequential version. This is useful to avoid spawning too small parallel computations and reduces communications overhead. `threshold` or `th` parameters in source code tune thresholds.

## Repa flat parallelism
Repa doesn't support nested data parallelism, while divide-and-conquer heavily rely on such a feature to exploit parallelism easily. My solution is based on the fact that nested parallelism uses a threshold, after which it switches to sequential version. I call one computation after the threshold a **tail**, and I say they're **sequential tail** when the evaluation of _a single_ tail is sequential (even though many tails may be evaluated in parallel).
With Repa I sequentially compute values until the threshold, then evaluates in parallel all the sequential tails.
To see an example with this approach described in details see `queens`; other benchmarks using it aren't.
A big difference with these two approaches is that while nested data parallelism can compute _in parallel_ starting points of each tail, Repa has to do it sequentially, and only then compute in parallel each sequential tail.
This approach will be referred as `sequential + parallelized sequential tails` in source code comments.
