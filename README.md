# Usage

We developed a perl script to automatically compile & run the test.

Example：
`./run.pl -clean order.imp`

`-clean` indicates cleaning compiliation garbage and results from previous run
`order.imp` is the benchmark that applied to minidafny, both relative and absulote path are welcome.

`./run.pl -all`

`-all` indicates running all the benchmarks under the Benchmarks director. The script will enumerate them and run them one by one.

# Collaborator

Yu Zhang
Bowen Huang