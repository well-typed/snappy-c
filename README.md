![snappy-c logo](./.github/assets/snappy-c.gif)

![CI Status Badge](https://github.com/FinleyMcIlwaine/snappy-c/actions/workflows/haskell-ci.yml/badge.svg)

Haskell bindings to [snappy][snappy]: A fast
compression/decompression library.

# Usage

> [!IMPORTANT]
> To use this package, you must have the snappy library installed **and visible
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

We wouldn't need to do this if the snappy library supported pkg-config
configuration, but as of writing [they do
not](https://github.com/google/snappy/pull/86#issuecomment-552237257).

## Installing `snappy`

The [snappy][snappy] library is available most package managers.

### Homebrew

```
brew install snappy
```

### APT

```
apt-get install libsnappy-dev
```

<!-- Links -->
[snappy]: https://github.com/google/snappy
