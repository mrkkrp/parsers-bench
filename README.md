# Parsers bench

[![License BSD3](https://img.shields.io/badge/license-BSD3-brightgreen.svg)](http://opensource.org/licenses/BSD-3-Clause)

This repo contains a collection of identical parsers implemented in
Attoparsec and Megaparsec from its current master branch. The purpose here
is to learn how much Attoparsec is actually faster than Megaparsec for
common parsing tasks. We do not focus on microbenchmarks here. The
benchmarks will be used to guide design of Megaparsec 6.

To generate Criterion report with stack, execute:

```bash
$ stack bench parsers-bench:bench-speed --benchmark-arguments=-oreport.html
$ stack bench parsers-bench:bench-memory
```

## License

Copyright © 2017–2018 Mark Karpov

Distributed under BSD 3 clause license.
