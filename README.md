# snappy-c

![CI Status Badge](https://github.com/well-typed/snappy-c/actions/workflows/haskell-ci.yml/badge.svg)

Haskell bindings to the C API of [Snappy][snappy]: A fast compression library.

# Usage

> [!IMPORTANT]
> To use this package, you must have the Snappy library installed **and visible
> to `cabal`**.

If your build fails with a message like:
```
Error: cabal-3.10.1.0: Missing dependency on a foreign library:
* Missing (or bad) C library: snappy
```

You need to explicitly configure the include and library paths via cabal. One
way to do so is to add something like this to your `cabal.project`
configuration:

```cabal
package snappy-c
  extra-include-dirs:
    /path/to/snappy/include
  extra-lib-dirs:
    /path/to/snappy/lib
```

In my case, on a mac using homebrew, the following suffices:

```cabal
package snappy-c
  extra-lib-dirs:
    /opt/homebrew/lib
  extra-include-dirs:
    /opt/homebrew/include
```

We wouldn't need to do this if the Snappy library supported pkg-config
configuration, but as of writing [they do
not](https://github.com/google/snappy/pull/86#issuecomment-552237257).

## Installing Snappy

The [Snappy][snappy] library is available most package managers.

### Homebrew

```
brew install snappy
```

### APT

```
apt-get install libsnappy-dev
```


# Performance

snappy-c stays true to the speedy spirit of Snappy compression.

The package includes a benchmark suite that compares the compression,
decompression, and roundtrip performance of this package against
- a [pre-existing (unmaintained) Snappy
  implementation](https://hackage.haskell.org/package/snappy), and
- the [zlib package's](https://hackage.haskell.org/package/zlib) zlib and gzip
  compression format implementations.

Run the benchmarks with:
```
cabal run bench-snappy-c
```

> [!IMPORTANT]
> To build the benchmarks, you will need to set `extra-include-dirs` and
> `extra-lib-dirs` for the `snappy` package as you did for `snappy-c` (see
> [Usage](#usage)).

Some sample results (where
[snappy-lazy](https://hackage.haskell.org/package/snappy-lazy) is a wrapper
around the pre-existing unmaintained implementation):

```
benchmarking compression/snappy-c
time                 17.14 ms   (17.12 ms .. 17.18 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 17.11 ms   (17.08 ms .. 17.12 ms)
std dev              49.09 μs   (33.43 μs .. 84.28 μs)

benchmarking compression/snappy-lazy
time                 483.1 ms   (481.6 ms .. 484.4 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 483.6 ms   (483.2 ms .. 483.9 ms)
std dev              414.9 μs   (183.6 μs .. 526.6 μs)
variance introduced by outliers: 19% (moderately inflated)

benchmarking compression/zlib
time                 963.3 ms   (956.5 ms .. 969.5 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 963.3 ms   (962.1 ms .. 965.0 ms)
std dev              1.687 ms   (540.0 μs .. 2.278 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking compression/gzip
time                 952.6 ms   (942.5 ms .. 963.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 952.4 ms   (950.9 ms .. 953.8 ms)
std dev              1.934 ms   (925.0 μs .. 2.641 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking decompression/snappy-c
time                 12.71 ms   (12.67 ms .. 12.75 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 12.73 ms   (12.70 ms .. 12.75 ms)
std dev              57.88 μs   (40.13 μs .. 86.41 μs)

benchmarking decompression/snappy-lazy
time                 186.6 μs   (186.1 μs .. 187.1 μs)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 187.2 μs   (186.9 μs .. 187.8 μs)
std dev              1.286 μs   (681.1 ns .. 2.329 μs)

benchmarking decompression/zlib
time                 21.05 ms   (20.97 ms .. 21.12 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 21.09 ms   (21.06 ms .. 21.16 ms)
std dev              99.24 μs   (43.40 μs .. 187.6 μs)

benchmarking decompression/gzip
time                 10.28 ms   (10.25 ms .. 10.31 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 10.31 ms   (10.29 ms .. 10.32 ms)
std dev              40.31 μs   (26.53 μs .. 62.17 μs)

benchmarking roundtrip/snappy-c
time                 29.21 ms   (29.13 ms .. 29.34 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 29.23 ms   (29.20 ms .. 29.29 ms)
std dev              96.53 μs   (59.39 μs .. 169.9 μs)

benchmarking roundtrip/snappy-lazy
time                 483.3 ms   (481.9 ms .. 485.7 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 482.3 ms   (481.0 ms .. 483.0 ms)
std dev              1.214 ms   (70.62 μs .. 1.493 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking roundtrip/zlib
time                 989.2 ms   (984.2 ms .. 999.9 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 995.4 ms   (992.8 ms .. 998.1 ms)
std dev              3.231 ms   (1.250 ms .. 4.049 ms)
variance introduced by outliers: 19% (moderately inflated)

benchmarking roundtrip/gzip
time                 973.3 ms   (971.6 ms .. 975.4 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 972.7 ms   (972.3 ms .. 973.0 ms)
std dev              428.5 μs   (189.3 μs .. 552.9 μs)
variance introduced by outliers: 19% (moderately inflated)
```

## `snappy-cli` performance

Compressing and decompressing the the [enwik9 test
data](https://mattmahoney.net/dc/textdata.html) (~953MB) 5 times in a minimally
controlled yet consistent environment resulted in the following average times:

| Tool         | Compress time (s) | Decompress time (s)  |
| ------------ | ----------------- | -------------------- |
| `snappy-cli` | 2.074             | 0.80                 |
| `snzip`      | 2.312             | 1.07                 |
| `gzip`       | 26.51             | 1.38                 |

Note that the runtimes of `snappy-cli` and `snzip` are dominated by IO. See
[snappy-cli/README.md](./snappy-cli/README.md) for more information on how these
numbers were collected.

<!-- Links -->
[snappy]: https://github.com/google/snappy
