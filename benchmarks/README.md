# Join Patterns Benchmarks

This directory contains benchmarks for the join patterns implementation. In particular,
we measure the performance of two matching algorithms: a brute-force algorithm and a
stateful tree-based algorithm.


For instance, to run the Smart House benchmark, run the following command:

```bash
sbt "benchmarks/run smart-house-config -n SmartHouse --warmup-repetitions 5 --repetitions 5 --write-to-file -m 1000 -r 32 -s 4"
```

The above command for instance shows how to run the bounded buffer benchmark
with the specified parameters. If the `--write-to-file` flag is provided, the
results will be written to a file in the default `benchmarks/data` directory. The
generated data is in CSV format and will be saved in directories named after the
benchmark name prefixed with the current date and time. If you want to specify
the output directory for the results, you can use the `-p` flag followed by the
path to the directory relative to the root of the project. For example,

```bash
sbt "benchmarks/run smart-house-config -n SmartHouse --warmup-repetitions 5 --repetitions 5 --write-to-file -m 1000 -r 32 -s 4 -p ../my/path/data"
```

will save the results in the `my/path/data` directory.

The following are the available benchmarks:
```bash
sbt "benchmarks/run size-config -n SizeNoGuard --warmup-repititions 1 --repititions 2 --write-to-file -m 5"
sbt "benchmarks/run size-with-noise-config -n SizeNoGuardsWithNoise --warmup-repititions 1 --repititions 2 --write-to-file -m 5"

sbt "benchmarks/run size-with-guards-config -n SizeWithGuardsNoNoise --warmup-repititions 1 --repititions 2 --write-to-file -m 5"
sbt "benchmarks/run size-with-guards-with-noise-config -n SizeWithGuardsWithNoise --warmup-repititions 1 --repititions 2 --write-to-file -m 5"
sbt "benchmarks/run size-with-guards-with-non-matching-payloads-config -n SizeWithGuardsWithNonMatchingPayloads --warmup-repititions 1 --repititions 2 --write-to-file -m 2"

sbt "benchmarks/run bounded-buffer-config -n BoundedBuffer --warmup-repititions 1 --repititions 2 --write-to-file -b 20 -p 4"

sbt "benchmarks/run smart-house-config -n SmartHouse --warmup-repetitions 5 --repetitions 5 --write-to-file -m 1000 -r 32 -s 4"
```

The parameters for each benchmark can be changed.
