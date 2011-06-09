# synthres

Synthesize an arbitrary resistance using a bunch of unit resistors of a given size.

## How it works

Imagine that I'm trying to make a resistance R from a bunch of unit resistors of size R*u*. (This is a standard technique for making well matched resistors on integrated circuits, where the mismatch properties of the resistor is dependent on its geometry.)

To do this, first normalize the desired resistance by the unit resistor. The integer portion of quotient gives you a number of unit resistors to connect in series. (For example, if I'm trying to make 10k out of 3k units, 10/3 gives 3 1/3, so I first connect 3 units in series.) Now I've just got the remainder; if I invert this, I get a normalized conductance value, the integer portion of which tells me how many units to connect in parallel. Now I once again invert the remainder, and I connect this many series units in parallel with the previous parallel branches, then invert the remainder and connect this many parallel units in series with the previous series branch, et cetera.

Let's say I want to make 8.7k out of 2k units.

    8.7 / 2 = 4.35 (4 2k units in series)
    1 / 0.35 = 2.8571 (2 parallel 2k units, in series with the above branch)
    1 / 0.8571 = 1.166667 (1 series 2k unit, in parallel with the above branch)
    1 / 0.16667 = 6 (6 parallel 2k units, in series with above branch)

That looks like this:

![8.7k resistor](https://github.com/kwantam/synthres/raw/master/8.7kres.gif)

## How to use it

    Usage:
    ./synthres.pl <unit> <resistance> [precision]
            unit and resistance are required, precision defaults to 1e-6

The output looks like this:

    $ ./synthres.pl 2000 8700
    (4x+(1/2x||(1x+(1/6x)))) (13 units)

Read this as above: 4 resistors in series with the parallel combination of two resistors plus the series combination of one resistor and the parallel combination of six more.

## License

synthres is free software.  It comes without any warranty, to
to the extent permitted by applicable law.  You can redistribute it
and/or modify it under the terms of the Do What The Fuck You Want To
Public License, Version 2, as published by Sam Hocevar.  See
http://sam.zoy.org/wtfpl/COPYING for more details

