# Haskell

This is a Haskell response to the blog post https://jackmott.github.io/programming/2016/09/01/performance-in-the-large.html

## Special Considerations

In order to reflect the spirit of the benchmark I ended up including the [DeepSeq](https://hackage.haskell.org/package/deepseq) library and the BangPatterns language pragma to force evaluation of everything in the `loadWorld` and `updateChunks` steps.

Unfortunately this ends up with about 80% of the benchmark's time spent forcibly evaluating all of the data structures. Haskell is happiest when it's being lazy, so at this point I am giving up on fighting against the language's features to make it benchmark correctly in the naive version.

## NaiveGame0.hs

Naive Data.List implementation.

122 MB/s allocations
100ms to load the world
150ms per tick

```
   2,363,112,184 bytes allocated in the heap
   2,788,587,328 bytes copied during GC
      24,103,856 bytes maximum residency (63 sample(s))
         824,016 bytes maximum slop
              72 MB total memory in use (0 MB lost due to fragmentation)

                                     Tot time (elapsed)  Avg pause  Max pause
  Gen  0      4465 colls,     0 par    0.865s   0.890s     0.0002s    0.0025s
  Gen  1        63 colls,     0 par    0.723s   0.759s     0.0121s    0.0267s

  TASKS: 4 (1 bound, 3 peak workers (3 total), using -N1)

  SPARKS: 0 (0 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)

  INIT    time    0.000s  (  0.001s elapsed)
  MUT     time   19.295s  ( 19.588s elapsed)
  GC      time    1.589s  (  1.650s elapsed)
  EXIT    time    0.000s  (  0.006s elapsed)
  Total   time   20.922s  ( 21.244s elapsed)

  Alloc rate    122,469,638 bytes per MUT second

  Productivity  92.4% of total user, 91.0% of total elapsed
```
