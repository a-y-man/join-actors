# Join Patterns Benchmarks

The benchmark suite is used with the following SBT command structure:

```bash
benchmarks/run [benchmark name] [parameters]
```

If not run from within the SBT shell, the command must be enclosed in quotes:

```bash
sbt "benchmarks/run [benchmark name] [parameters]"
```

`benchmark_name` can be set to any of the following, allowing access to all implemented benchmarks:

- `simple-smart-house`
- `complex-smart-house`
- `bounded-buffer`
- `size`
- `size-with-guards`

All benchmarks have the following parameters in common:

- `algorithms`: the algorithm to use, the word `all`, or a comma-separated list of algorithms enclosed in quotes. 
  The `all` option uses all implemented algorithms, and this is the default option. For the other options, the 
  algorithms are written as follows (the same as when running examples in the core package):
  - `brute`
  - `stateful`
  - `mutable`
  - `lazy-mutable`
  - `while-lazy`
  - `while-eager`
  - `eager-parallel`
  - `lazy-parallel`
  - `filtering-while`
  - `filtering-parallel`
- `exclude`: an algorithm or comma-separated list of algorithms to exclude from the benchmark. The algorithms are written
  in the same way as above. Best used in combination with `--algorithms all`
- `min-param`: the minimum main parameter value to use
- `param-step`: the step by which the main parameter value should increase
- `max-param`: the maximum main parameter value to use
- `repetitions`: the number of repetitions to run for each main parameter value
- `warmup`: the number of repetitions from the benchmark to run as warmup repetitions before starting the real benchmark
- `path`: the file path to which to write the benchmark results

In addition, each benchmark has its own specific parameters:

**Simple Smart House**
- `matches`: The maximum number of matches the smart house actor should perform
- `heavyGuard`: Whether to use a heavy guard

**Complex Smart House**
- `matches`: The maximum number of matches the smart house actor should perform

**Bounded Buffer**
- `bufferBound`: The buffer bound
- `count`: The number of puts/gets performed by each producer and consumer

**Size**
- `matches`: The number of matches the size actor should perform
- `noise`: Whether to include noise in the messages

**Size with guards**
- `matches`: The number of matches the size actor should perform
- `variant`: The benchmark variant to run: either "normal", "noisy", or "non-matching"
