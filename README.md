# synthres

Synthesize an arbitrary resistance using a bunch of unit resistors of a given size.

There are two different implementations here: the original one in Perl, and a new one that searches for a more optimal solution (more below).

## How it works

Imagine that I'm trying to make a resistance R from a bunch of unit resistors of size R*u*. (This is a standard technique for making well matched resistors on integrated circuits, where the mismatch properties of the resistor is dependent on its geometry.)

To do this, first normalize the desired resistance by the unit resistor. The integer portion of this quotient gives you a number of unit resistors to connect in series. (For example, if I'm trying to make 10k out of 3k units, 10/3 gives 3 1/3, so I first connect 3 units in series.) Now I've just got the remainder; if I invert this, I get a normalized conductance value, the integer portion of which tells me how many units to connect in parallel. Now I once again invert the remainder, and I connect this many series units in parallel with the previous parallel branches, then invert the remainder and connect this many parallel units in series with the previous series branch, et cetera.

Let's say I want to make 8.7k out of 2k units.

    8.7 / 2 = 4.35 (4 2k units in series)
    1 / 0.35 = 2.8571 (2 parallel 2k units, in series with the above branch)
    1 / 0.8571 = 1.166667 (1 series 2k unit, in parallel with the above branch)
    1 / 0.16667 = 6 (6 parallel 2k units, in series with above branch)

That looks like this:

![8.7k resistor](https://github.com/kwantam/synthres/raw/master/8.7kres.gif)

## Optimization

The above is guaranteed to converge on *some* value, but not necessarily an optimal one. Take, for example, making R=1.2 from R*u*=1. The above algorithm gives R+R/5, which takes 6 resistors to implement. The same value can be implemented with 3R||2R, which is only 5 resistors.

So, how do we go about finding more optimal values for R? Certainly, the above algorithm gives an upper bound on the number of resistors necessary; from there, we can imagine generating various series-parallel combinations of resistors using up to the bounding value.

The set of "simple" series-parallel combinations of *n* resistors---that is, the combinations that can be created by connecting series strings in parallel---is identical to the set of integer partitions of *n*, where the addition operator is replaced by the parallel operator; let's call such networks *partition networks*.  For example, if *n*=4, the partitions are:

    4
    3+1
    2+2
    2+1+1
    1+1+1+1

Thus, the partition networks are:

    4          = 4
    3||1       = 3/4
    2||2       = 1
    2||1||1    = 2/5
    1||1||1||1 = 1/4

None of the partition networks of 4 are any more optimal than the networks given by the algorithm in the previous section, but partition networks for larger numbers can be smaller than their algorithmically generated counterparts (e.g., R/R*u*=6/5 listed above).

Given that we can generate bounded sets of candidate optimizations for our resistor network, we can easily check whether they produce a better result than the naive algorithm: first, generate a bound on the network, and then successively test candidates from the set of partition networks until we find something better than the naive result. If we don't, just return that instead. Obviously, this becomes a recursive problem: if I'm trying to make R/R*u*=3.9, I can add together several smaller networks, e.g., 2/5 + 3 + 1/2, which implements in 9 resistors what the naive algorithm makes in 13 (3 + 1||9).

The number of partitions of *n* grows rather fast: e^sqrt(n) per [Wolfram Mathworld](http://mathworld.wolfram.com/PartitionFunctionP.html). However, we can usually reduce this number substantially by noting that the largest-valued nontrivial partition network of *n* resistors has value less than or equal to *n*/4 (for even numbers, it is exactly *n*/4; for odd numbers, it's (*n*^2-1)/(4*n*)). Thus, if the resistance value we're trying to synthesize is greater than *n*/4, we can immediately pull out most of the resistance as a series network and only optimize the remainder. Note that this only works for *resistance* synthesis: the maximum *conductance* that can be synthesized from a partition network of *n* resistors is *n*, so the only optimization we can do is to note that we can never synthesize a larger conductance that the upper bound on the number of resistors.

## Generating all networks of size N

Another interesting question might be: what is every resistor value that can be synthesized, given a network of *n* unit resistors?

The answer once again has to do with the integer partitions of *n*. If I recursively partition *n*, then combine every resistor into its constituent series/parallel network, at the end what must result is every possible network that can be made from arbitrarily complex series/parallel combinations of *n* resistors. The `allResNets` function in `ResNetSynth.hs` does exactly this; `AllResNetsCGI.hs` implements a CGI interface to this function. Note that computation time grows *very* quickly with *n*, at least in my quickie implementation. There's plenty of room for optimization.

## How to use it

### synthres.pl

    Usage:
    ./synthres.pl <unit> <resistance> [precision]
            unit and resistance are required, precision defaults to 1e-6

The output looks like this:

    $ ./synthres.pl 2000 8700
    (4x+(1/2x||(1x+(1/6x)))) (13 units)

Read this as above: 4 resistors in series with the parallel combination of two resistors plus the series combination of one resistor and the parallel combination of six more.

### synthres.hs

    $ make
    (...stuff...)
    $ build/synthres 2000 8700
    SRes (ResM 4,PRes (SRes (IntP [1,2],IntP [1,1]),IntP [1,1]))
    11
    87 % 20 (4.35)
    E_rel = 0.0
    <svg xmlns=(...etc...)

synthres.hs dumps out three informational lines on stderr and then renders the network on stdout. Here's what the resulting SVG looks like for the above:

![8.7k resistor, optimized](https://github.com/kwantam/synthres/raw/master/8.7kres_optimal.gif)

### CGI interfaces

`SynthResCGI.hs` and `AllResNetsCGI.hs` are CGI interfaces to the generator. They present the user with the appropriate prompt and generate the network. Note that either of these can be used to render arbitrary resistor networks as SVG by passing a serialized `ResNet` datatype (see `ResNetType.hs`).

## License

synthres is free software.  It comes without any warranty, to
to the extent permitted by applicable law.  You can redistribute it
and/or modify it under the terms of the Do What The Fuck You Want To
Public License, Version 2, as published by Sam Hocevar.  See
http://sam.zoy.org/wtfpl/COPYING for more details

