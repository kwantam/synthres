#!/usr/bin/perl

# 
# Synthesize an arbitrary resistance from a base unit
# Eventually this could also give mean and variance numbers
#
# synthres is free software.  It comes without any warranty, to
# to the extent permitted by applicable law.  You can redistribute it
# and/or modify it under the terms of the Do What The Fuck You Want To
# Public License, Version 2, as published by Sam Hocevar.  See
# http://sam.zoy.org/wtfpl/COPYING for more details
#

my $unit = $ARGV[0];
my $res  = $ARGV[1];
my $precision = $ARGV[2] || 1e-3;

unless ($unit && $res)
{
    print "Usage:\n$0 <unit> <resistance> [precision]\n\tunit and resistance are required, precision defaults to 0.1%\n";
    exit(-1);
}

sub calcres()
{
    my $unit = $_[0];
    my $res  = $_[1];
    my $prec = $_[2] || $precision;
    my @result;

    return undef unless ($unit && $res);

    my $norm = $res/$unit;

    for ($i=0;;$i++)
    {
        if (int($norm+$prec) != int($norm)) {
            $whole = int($norm) + 1;
            $norm = 0;
        } else {
            $whole = int($norm);
            $norm = $norm - $whole;
        }

        push @result, $whole;

        last if ($norm<=$prec);

        # we always want precision referred to the total resistance,
        # so it gets bigger as we start synthesizing smaller pieces
        $prec = $prec * ($norm + $whole) / $norm;

        $norm = 1/$norm;
    }

    return \@result;
}

sub printres()
{
    my @resistors = @{$_[0]};
    my $restot;
    return undef unless @resistors;

    for ($i=0;;$i++)
    {
        $restot += $resistors[$i];

        print "(" . ($i%2?"1/":"") . $resistors[$i] . "x";

        last if $i==$#resistors;

        print ($i%2?"||":"+");
    }

    print ")"x($i+1) . " (" . $restot . " units)\n";
}

&printres(&calcres($unit,$res));

