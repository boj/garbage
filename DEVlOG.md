

[2016.09.14] {Adding a Repa version}
------------------------------------

On swarm (Xeon CPU E5-2699 v3 @ 2.30GHz), I'm seeing these tick times
reported by criterion:

 * -N1   571 μs
 * -N2   375 μs
 * -N4   296, productivity 92%
 * -N6   373, productivity 87%
 * -N8   395, productivity 83%
 * -N12  417, 74%
 * -N16  426, 66%
 * -N18  444, 76%

Criterion's reporting nice R^2 values (0.999-1.0).  I'm also seeing
~25 ms initial load times.

Ran with:

    stack exec garbage-repa -- +RTS -s -A20M -N1 -RTS
