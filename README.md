# Usage

We developed a perl script to automatically compile & run the test.

Example：

`./run.pl -clean order.imp`

`-clean` indicates cleaning compiliation garbage and results from previous run
`order.imp` is the benchmark that applied to minidafny, both relative and absulote path are welcome.

`./run.pl -all`

`-all` indicates running all the benchmarks under the Benchmarks director. The script will enumerate them and run them one by one.

`./run.pl -info=5 -all`

`info=5` indicates printing all the compilation, execution and debug information.

Running the perl script will re-compile the scala source file every time. In case you have compiled it and do not want to waste time waiting compiling, you can run the bytecode directly:

`scala -cp _Objs VCGen path/to/imp/file`

# Collaborators

Yu Zhang and Bowen Huang

# Benchmarks

We supplied new benchmarks and they can be found in the directory `Benchmark/valid`

* `arr_order.imp`: Given two arrays `a` and `b`, swap `a[i]` and `b[i]` if `a[i] > b[i]`. Thus, for all index `i`, `a[i] <= b[i]`. Simple and trivial.
* `arr_order_par.imp`: An implementation of `arr_order.imp` using parallel array write.
* `bsearch.imp`: Binary search.
* `bubble_improved.imp`: A slightly optimized version of bubble sort so the invariants are slightly more complicated. Parallel array write is used.
* `bubble_parwrite.imp`: The same as `bubble.imp` except parallel array write is used to swap two elements.
* `insertion.imp`: Insertion sort.
* `max.imp`: Find the maximum element in an array.
* `order_corner_case.imp`: Test of the corner cases, e.g. a while loop with empty body, etc.
* `order_empty_forall.imp`: Test of the corner cases, e.g. a forall formula with no quantified variables, etc.
* `partition.imp`: Array Partition. Parallel array write is used.
* `rev_rev.imp`: Reverse an array twice.
* `selection.imp`: Selection sort.

All the benchmarks above can be verified. There is one more interesting benchmark in the directory `Benchmark/valid`

* `gcd_euclidean.imp`: An euclidean algorithm implementation for GCD problem. I believe the program is valid but z3 cannot figure it out. So it will run for a long while and return `unknown`. Thus, the output of our verifies for this test case is `Not Verified`.

# Implementation of VCG
* Object `ImpParser`: A parser that parses the program with its pre- and post-conditions.
* Object `VCGenerator`: A generator that generate the weakest pre-condition directly from the program and its post-conditions. The intermidiate guard condition language is bypassed and the problem of generating fresh variable names is also avoided. 
* Object `SMTFormat`: A translator that translates the assertions to SMT2 format formulars.
* Object `Helper`: Auxilary functions.
* Object `PrettyPrinter`: A simple pretty printer. This part is not working very well and the output is still a little messy. It's only used for the purpose of debug.

