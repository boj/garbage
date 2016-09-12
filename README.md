# Haskell

This is a Haskell response to the blog post https://jackmott.github.io/programming/2016/09/01/performance-in-the-large.html

## NaiveGame0.hs

Naive Data.List implementation.

In order to reflect the spirit of the benchmark I ended up including the [DeepSeq](https://hackage.haskell.org/package/deepseq) library and the BangPatterns language pragma to force evaluation of everything in the `loadWorld` step and the `updateChunks` step in the main loop. Unfortunately this ends up imposing a terrible performance penalty itself.

## NaiveGame1.hs

Slightly less naive Linear and Data.Sequence implementation.

DeepSeq appears to impose same penalty as Data.List solution.
