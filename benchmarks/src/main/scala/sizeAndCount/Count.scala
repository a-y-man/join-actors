// package test.benchmark.sizeCount

// import test.classes.sizeCount.counts.*
// import test.classes.sizeCount.Size1Count1
// import test.classes.Msg
// import test.benchmark.{Benchmark, BenchmarkPass}
// import test.classes.sizeCount.{A, B, C, D, E, F, G, H, I, J}
// import scala.util.Random
// import test.ALGORITHM
// @main
// def countBenchmark =
//   val maxHits = 3_000

//   Benchmark(
//     "Count",
//     10,
//     100,
//     BenchmarkPass(
//       s"Count1 using ${ALGORITHM.toString()}",
//       () =>
//         val actor  = Size1Count1(maxHits)
//         val result = actor.run_as_future

//         for _ <- 0 to maxHits do actor.ref.send(A())

//         result
//     ),
//     List(
//       BenchmarkPass(
//         s"Count2 using ${ALGORITHM.toString()}",
//         () =>
//           val actor  = Count2(maxHits)
//           val result = actor.run_as_future

//           for _ <- 0 to maxHits do
//             actor.ref.send {
//               Random.nextInt(2) match
//                 case 0 => A()
//                 case 1 => B()
//             }

//           result
//       ),
//       BenchmarkPass(
//         s"Count3 using ${ALGORITHM.toString()}",
//         () =>
//           val actor  = Count3(maxHits)
//           val result = actor.run_as_future

//           for _ <- 0 to maxHits do
//             actor.ref.send {
//               Random.nextInt(3) match
//                 case 0 => A()
//                 case 1 => B()
//                 case 2 => C()
//             }

//           result
//       ),
//       BenchmarkPass(
//         s"Count4 using ${ALGORITHM.toString()}",
//         () =>
//           val actor  = Count4(maxHits)
//           val result = actor.run_as_future

//           for _ <- 0 to maxHits do
//             actor.ref.send {
//               Random.nextInt(4) match
//                 case 0 => A()
//                 case 1 => B()
//                 case 2 => C()
//                 case 3 => D()
//             }

//           result
//       ),
//       BenchmarkPass(
//         s"Count5 using ${ALGORITHM.toString()}",
//         () =>
//           val actor  = Count5(maxHits)
//           val result = actor.run_as_future

//           for _ <- 0 to maxHits do
//             actor.ref.send {
//               Random.nextInt(5) match
//                 case 0 => A()
//                 case 1 => B()
//                 case 2 => C()
//                 case 3 => D()
//                 case 4 => E()
//             }

//           result
//       ),
//       BenchmarkPass(
//         s"Count6 using ${ALGORITHM.toString()}",
//         () =>
//           val actor  = Count6(maxHits)
//           val result = actor.run_as_future

//           for _ <- 0 to maxHits do
//             actor.ref.send {
//               Random.nextInt(6) match
//                 case 0 => A()
//                 case 1 => B()
//                 case 2 => C()
//                 case 3 => D()
//                 case 4 => E()
//                 case 5 => F()
//             }

//           result
//       ),
//       BenchmarkPass(
//         s"Count7 using ${ALGORITHM.toString()}",
//         () =>
//           val actor  = Count7(maxHits)
//           val result = actor.run_as_future

//           for _ <- 0 to maxHits do
//             actor.ref.send {
//               Random.nextInt(7) match
//                 case 0 => A()
//                 case 1 => B()
//                 case 2 => C()
//                 case 3 => D()
//                 case 4 => E()
//                 case 5 => F()
//                 case 6 => G()
//             }

//           result
//       ),
//       BenchmarkPass(
//         s"Count8 using ${ALGORITHM.toString()}",
//         () =>
//           val actor  = Count8(maxHits)
//           val result = actor.run_as_future

//           for _ <- 0 to maxHits do
//             actor.ref.send {
//               Random.nextInt(8) match
//                 case 0 => A()
//                 case 1 => B()
//                 case 2 => C()
//                 case 3 => D()
//                 case 4 => E()
//                 case 5 => F()
//                 case 6 => G()
//                 case 7 => H()
//             }

//           result
//       ),
//       BenchmarkPass(
//         s"Count9 using ${ALGORITHM.toString()}",
//         () =>
//           val actor  = Count9(maxHits)
//           val result = actor.run_as_future

//           for _ <- 0 to maxHits do
//             actor.ref.send {
//               Random.nextInt(9) match
//                 case 0 => A()
//                 case 1 => B()
//                 case 2 => C()
//                 case 3 => D()
//                 case 4 => E()
//                 case 5 => F()
//                 case 6 => G()
//                 case 7 => H()
//                 case 8 => I()
//             }

//           result
//       ),
//       BenchmarkPass(
//         s"Count10 using ${ALGORITHM.toString()}",
//         () =>
//           val actor  = Count10(maxHits)
//           val result = actor.run_as_future

//           for _ <- 0 to maxHits do
//             actor.ref.send {
//               Random.nextInt(10) match
//                 case 0 => A()
//                 case 1 => B()
//                 case 2 => C()
//                 case 3 => D()
//                 case 4 => E()
//                 case 5 => F()
//                 case 6 => G()
//                 case 7 => H()
//                 case 8 => I()
//                 case 9 => J()
//             }

//           result
//       )
//     )
//   ).run
