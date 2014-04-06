#! /usr/bin/perl -w

use strict;

my @a;
while (<>) {
  chomp;
  push @a,$_;
}

print "$_\n" for shuffle(@a);

sub shuffle {
  for my $i (0..@_-1) {
    my $j = int rand($i+1);
    @_[$i,$j] = @_[$j,$i] unless $i==$j;
  }
  return @_;
}
