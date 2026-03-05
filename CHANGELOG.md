# Revision history for snappy-c

## 0.1.3 -- 2026-03-05

* Optional `pkg-config` support [#8, Alex Biehl]

  There is no official `pkg-config` support for `snappy`
  (https://github.com/google/snappy/pull/86#issuecomment-552237257)
  but some distributions (including `nixpkgs`) include an inofficial one.

* Relax bounds for ghc 9.14.

## 0.1.2 -- 2025-11-15

* Depend on `crc32c` rather than `digest` [#6, Mateusz Galazyn]

## 0.1.1 -- 2024-04-10

* Lower bound on `bytestring` to 0.10

## 0.1.0 -- 2024-02-09

* First public release
