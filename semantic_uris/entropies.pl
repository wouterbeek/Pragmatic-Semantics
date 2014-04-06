#! /usr/bin/perl -w
#
# Calculates all kinds of interesting information theoretic quantities
# between the various entries in the given sequence of input vectors.
#
# Conventions: data is an array of input vectors. We want to know the entropy of,
# and conditional entropy between, all except the first two fields in these input vectors.
# The mutual information is also output.
# 
# $x and $y are the vector index of which we want to know the entropy and the vector index
# on which we condition, respectively.

use strict;

# 1. Read @data array

my @data;

my @fieldnames = qw/type ix property_set type_set fingerprint partition_cell/;
my %fieldname2ix;
$fieldname2ix{$fieldnames[$_]} = $_ for 0..@fieldnames-1;

while (<>) {
  chomp;
  s/\s*\#.*//; # remove comments
  $_ eq "" and next;

  my @fields = split /\,/;
  @fields == @fieldnames or die "Incorrect # fields in data: '$_'\n";
  push @data, \@fields;
}

# 2. Calculate all relevant tables and print them

my @kind2name = ("URIs", "BNodes", "Both URIs and BNodes");
for my $kind (0...2) { 
  my %filter;
  $kind==0 and $filter{0} = "URI";
  $kind==1 and $filter{0} = "BNODE";
  my ($H, $Hcond) = compute_table(filter(\@data, \%filter));
  print "\n\n================ ", $kind2name[$kind], " =================\n";
  print_tables($H, $Hcond);
}

# End of main program



# Usage:  ($H,$Hcond)=compute_table($data)
# Result: $H->[$x] is the entropy of the $x-th data vector entry
#         $Hcond->[$y][$x] is the entropy of the $x-th data vector entry, 
#                          conditional on the y-th
# Note it is useful to store these tables, because they are reused to calculate
# the mutual information 
sub compute_table {
  my $data = shift;
  my (@H, @Hcond);
  for my $y (2..@fieldnames-1) {
    $Hcond[$y][$y]=0;
    for my $x (2..@fieldnames-1) {
      $y == $x and next;
      my ($Hcond, $H) = conditional_entropy($data, $x, $y);
      $Hcond[$y][$x] = $Hcond;
      $H[$y] = $H;
    }
  }
  return (\@H, \@Hcond);
}

# Nicely output entropy and mutual information tables
sub print_tables {
  my ($H, $Hcond) = @_;
  print "* Entropy and conditional entropy\n";
  for my $x (2..@fieldnames-1) {
    printf "%-15s | %5.3f | ", $fieldnames[$x], $H->[$x];
    for my $y (2..@fieldnames-1) {
      printf "%5.3f ", $Hcond->[$y][$x];
    }
    print "\n";
  }

  print "\n* Mutual information\n";
  for my $x (2..@fieldnames-1) {
    printf "%-15s | ", $fieldnames[$x];
    for my $y (2..@fieldnames-1) {
      printf "%5.3f ", $H->[$x] - $Hcond->[$y][$x];
    }
    print "\n";
  }
}

# Usage: ($Hcond, $H)=conditional_entropy($data, $x, $y)
# Result: $H     is the entropy of vector entry $y (NOTE: not $x!)
#         $Hcond is the conditional entropy of vector entry $x given entry $y
sub conditional_entropy {
  my ($data, $x, $y) = @_;
  my ($cond2Pfield, $Pcond) = P($data, $x, $y);
  my $Hcond = 0;
  while (my ($y, $py) = each %$Pcond) {
    $Hcond += $py * H($cond2Pfield->{$y});
  }
  return ($Hcond, H($Pcond));
}


# Usage:  $H=H($P), where $P is a probability distribution in hash ref representation
# Result: $H is the entropy of $P
sub H {
  my $P = shift;
  my $H = 0;
  $H -= $_*log($_)/log(2) for values %$P;
  return $H;
}

# Usage: ($y2Px, $Py)=P($data, $x, $y)
# Result: $y2Px{$val} is the distribution of vector field $x given that the value of field
#         y is val
#         $Py is the distribution of the vector field $y
# The distributions are hash references with outcomes as keys and the corresponding probs
# as values
sub P {
  my ($data, $x, $y) = @_;
  my %y2Px;
  my %N;
  my $n = 0;
  for my $item (@$data) {
    $y2Px{$item->[$y]}{$item->[$x]} ++;
    $N{$item->[$y]}++;
    $n++;
  }
  # normalize all distributions and construct the marginal
  my %Py;
  for my $x (keys %y2Px) {
    my $P_given_x = $y2Px{$x};
    my $n_given_x = $N{$x};
    $Py{$x} = $n_given_x / $n;
    $_/=$n_given_x for values %$P_given_x;
  }
  return (\%y2Px, \%Py);
}

sub filter {
  my ($data, $y) = @_;
  my @res;
 ITEM: for my $item (@$data) {
    $item->[$_] ne $y->{$_} and next ITEM for keys %$y;
    push @res, $item;
  }
  return \@res;
}
