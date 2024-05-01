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
results will be written to a file in the `benchmarks/data` directory. The
generated data is in CSV format and will be saved in directories named after the
benchmark name prefixed with the current date and time.
