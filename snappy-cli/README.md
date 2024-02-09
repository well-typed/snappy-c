# `snappy-cli`

A proof-of-concept CLI application Snappy compression/decompression.

To see available options:
```
$ cabal run snappy-cli -- --help
```

## Sanity checking

This tool was written in-part to ensure that compressing/decompressing large
files was done in constant space. The large test file used was the [enwik9 test
data](https://mattmahoney.net/dc/textdata.html) for the [Large Text Compression
Benchmark](https://mattmahoney.net/dc/text.html) (~1 GB of text).

With the data in a file named `enwik9`, use `ls` to measure its uncompressed
size in bytes:
```
$ ls -l, enwik9 | awk '{print $5, $9}'
1,000,000,000 enwik9
```

> I'm on macOS. If you're on a different platform, you may need to modify the
> above command.

### snappy-c performance

Install `snappy-cli`:
```
$ cabal install snappy-cli
```

#### Compression

Compress the file with `snappy-cli` and output some RTS statistics by running:
```
$ time snappy-cli -i enwik9 -o enwik9.snappy-c.sz +RTS -s
   3,262,466,912 bytes allocated in the heap
         793,736 bytes copied during GC
         260,776 bytes maximum residency (2 sample(s))
          37,368 bytes maximum slop
              11 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       749 colls,     0 par    0.003s   0.004s     0.0000s    0.0001s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time    2.035s  (  2.046s elapsed)
  GC      time    0.003s  (  0.004s elapsed)
  EXIT    time    0.000s  (  0.009s elapsed)
  Total   time    2.039s  (  2.059s elapsed)

  Alloc rate    1,603,126,638 bytes per MUT second

  Productivity  99.8% of total user, 99.3% of total elapsed

snappy-cli -i enwik9 -o enwik9.snappy-c.sz +RTS -s  1.86s user 0.18s system 98% cpu 2.066 total
```
This indicates that compression runs in constant space (maximum residency is
low). To verify this further, make the tool output a GHC event log by adding the
`-l -hi` RTS flags and check that the residency profile looks constant.

Check the size of the compressed file with `ls`:
```
$ ls -l, enwik9.snappy-c.sz | awk '{print $5, $9}'
509,006,598 enwik9.snappy-c.sz
```

View the beginning of the compressed file to ensure it looks
Snappy frame encoded using `xxd`:
```
$ xxd enwik9.snappy-c.sz | head -n 5
00000000: ff06 0000 734e 6150 7059 009f 8f00 1660  ....sNaPpY.....`
00000010: 876f 8080 0470 3c6d 6564 6961 7769 6b69  .o...p<mediawiki
00000020: 2078 6d6c 6e73 3d22 6874 7470 3a2f 2f77   xmlns="http://w
00000030: 7777 2e15 1c50 2e6f 7267 2f78 6d6c 2f65  ww...P.org/xml/e
00000040: 7870 6f72 742d 302e 332f 2209 310c 3a78  xport-0.3/".1.:x
```

#### Decompression

Now decompress:
```
$ time snappy-cli -d -i enwik9.snappy-c.sz -o enwik9.snappy-c +RTS -s
   2,047,211,936 bytes allocated in the heap
         443,768 bytes copied during GC
         151,584 bytes maximum residency (2 sample(s))
          37,376 bytes maximum slop
              11 MiB total memory in use (0 MiB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0       472 colls,     0 par    0.002s   0.002s     0.0000s    0.0001s
  Gen  1         2 colls,     0 par    0.000s   0.000s     0.0002s    0.0002s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.001s  (  0.001s elapsed)
  MUT     time    0.525s  (  0.620s elapsed)
  GC      time    0.002s  (  0.003s elapsed)
  EXIT    time    0.000s  (  0.002s elapsed)
  Total   time    0.529s  (  0.625s elapsed)

  Alloc rate    3,896,096,952 bytes per MUT second

  Productivity  99.4% of total user, 99.2% of total elapsed

snappy-cli -d -i enwik9.snappy-c.sz -o enwik9.snappy-c +RTS -s  0.38s user 0.15s system 84% cpu 0.631 total
```
Decompression is, as expected, less costly than compression and still runs in
constant space.

The decompressed file should be identical to the uncompressed file. Check this
with `diff`:
```
$ diff enwik9 enwik9.snappy-c
```

### Comparison with `snzip`

#### Compression

Compress the enwik9 file with [`snzip`](https://github.com/kubo/snzip) (a
popular pre-existing CLI tool for snappy compression) like so:

```
$ time snzip -c enwik9 > enwik9.snzip.sz
snzip -c enwik9 > enwik9.snzip.sz  2.14s user 0.17s system 98% cpu 2.333 total
```

In our measurements, `snappy-cli` is consistently faster than `snzip` at
compressing this file, although both runtimes are dominated by IO. The
compressed files are also equivalent:
```
$ diff enwik9.snzip.sz enwik9.snappy-c.sz
```

#### Decompression

Decompress:
```
$ time snzip -d -c enwik9.snzip.sz > enwik9.snzip
snzip -d -c enwik9.snzip.sz > enwik9.snzip  0.88s user 0.16s system 97% cpu 1.063 total
```

Again, `snappy-cli` is consistently faster.

Both compressed files roundtrip identically when decompressed with either tool:
```
$ time snappy-cli -d -i enwik9.snzip.sz -o enwik9.snzip.snappy-c
snappy-cli -d -i enwik9.snzip.sz -o enwik9.snzip.snappy-c  0.60s user 0.16s system 93% cpu 0.811 total
$ time snzip -d -c enwik9.snappy-c.sz > enwik9.snappy-c.snzip
snzip -d -c enwik9.snappy-c.sz > enwik9.snappy-c.snzip  0.90s user 0.18s system 93% cpu 1.155 total
$ diff enwik9 enwik9.snzip.snappy-c
$ diff enwik9.snzip.snappy-c enwik9.snappy-c.snzip
```

### Comparison against `gzip`

Snappy is built for speed, so it's no surprise that default `gzip` compression
is much slower but results in a smaller result:

```
$ time gzip -c enwik9 > enwik9.gz
gzip -c enwik9 > enwik9.gz  25.37s user 0.12s system 98% cpu 25.786 total
$ ls -l, enwik9.gz | awk '{print $5, $9}'
323,941,227 enwik9.gz
```

Even `gzip`'s fastest setting is slower (but still compresses better):
```
$ time gzip -1 -c enwik9 > enwik9.1.gz
gzip -1 -c enwik9 > enwik9.1.gz  7.66s user 0.12s system 97% cpu 8.003 total
$ ls -l, enwik9.1.gz | awk '{print $5, $9}'
378,355,095 enwik9.1.gz
```
