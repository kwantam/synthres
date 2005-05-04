#!/usr/bin/perl

# 
# Synthesize an arbitrary resistance from a base unit
# Eventually this should also give mean and variance numbers
#

my $unit = $ARGV[0];
my $res  = $ARGV[1];
my $precision = $ARGV[2] || 1e-2;

unless ($unit && $res)
{
        print "Usage: $0 <unit> <resistance>\n";
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
        	$whole = int($norm);
        	$norm -= $whole;
		push @result, $whole;
	
        	last if ($norm<$prec || $i>10);
		
		# we always want precision referred to the total resistance,
		# so it gets bigger as we start synthesizing smaller pieces
		$prec = $prec / $norm;
	
        	$norm = 1/$norm;
	}
	
	return \@result;
}

sub printres()
{
	my @resistors = @{$_[0]};
	my $restot;
	return undef unless defined(@resistors);

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



