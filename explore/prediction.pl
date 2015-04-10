#!/usr/bin/env perl
use strict;
use warnings;

###
# Fills in missing values from https://www.kaggle.com/c/bike-sharing-demand
#
# Author: Stephen D. Wells
# Date: Apr, 4th 2015
# Ver: 1.0
#
# Usage: $0 > prediction.csv
#
# Loads all the test data and tosses away everything
# but the day (YYYYmmdd), hour, holiday and workingday values
#
# Then runs through the training data in a single pass and 
# builds out the results according the following rules:
#
# 1. reads in a line of training data and strips out everything
# except the day (YYYYmmdd), hour, holiday, workingday and count.
#
# 2. compares it's day to the current test day we are working
# on. If it's later than today we process it, otherwise we
# record it according to it's holiday and workingday values
# and move on to the next line.
#
# 3. If on the other hand, it's later than the current test day
# and it matches our holiday and working day we calculate the
# count values by using the following formula.
#
# Pv = Past Value (riders for that same hour)
# Fv = Future Value
# Pd = Past Days (number of days from our test day)
# Fd = Future Days (days into the future from our test day)
#
# Total_Change = Fv - Pv
# Total_Distance = Fd - Pd
# Change_per_Day = Total_Change / Total_Distance
#
# COUNT = (Change_per_Day * Pd) + Pv
#  ... or ...
# COUNT = Fd - (Change_per_Day * Fd)
#
# We are just taking the intervals and trying to get as
# close to it linearly between the closest sets of data that
# we have.

# Fill in the location of your training and test dataset
# expects the data to be named 'train.csv' and 'test.csv' zipped
#
use constant DATA_DIR => '../data/';

# Ensure this library is installed
use Date::Calc qw(Delta_Days);

#####  START ######
#
my ($train, $test) = fetch_data();

# gather up our test data
my @td;
{
  my $c = 0;
  for (split /[\r\n\cM]+/, $test) {
      next if (!$c && ++$c); # skip the header row

      my @line = split /,/, $_;

      # test data: day / hour / holiday / workingday
      push(@td, [format_day_hour($line[0]), $line[2], $line[3]]); 
  }
}

# print out our header
print '"datetime","count"',"\n";

# Mesh the two sets together
{
  my $test_day = shift(@td);
  my @past_value; # contains [holiday][workingday][hour] = [YYYYmmdd, count]
  my $c = 0; # to skip the header
  for (split /[\r\cM\n]+/, $train) {
      next if (!$c && ++$c); # skip the header row

      # input line
      my @line = split /,/, $_;

      # output: day, hour, holiday, workingday, count
      my($dy, $hr, $hol, $wrk, $count) = (
	    format_day_hour($line[0]),
	    $line[2],   # holiday
	    $line[3],   # workingday
	    $line[-1]); # count

      my($tdy, $thr, $thol, $twrk) = @$test_day;
      if ($test_day && $dy > $test_day->[0] && $hr == $thr && $wrk == $twrk) {
         my $past_count = $past_value[$thol][$twrk][$thr]->[1];
         my $past_distance = calculate_distance($past_value[$thol][$twrk][$thr]->[0], $test_day->[0]);
         my $future_distance = calculate_distance($test_day->[0], $dy);

	 # round the count using int(val + 0.5)
         my $final_count = int(calculate_count($past_count, $count, $past_distance, $future_distance) + 0.5);

         print join('-', unpack('A4A2A2', $test_day->[0])), ' ', $thr, ':00:00,', $final_count, "\n";

         $test_day = shift(@td)
      }

      $past_value[$hol][$wrk][$hr] = [$dy, $count];
  }
}

##### SUBROUTINES #####
#

# Inputs: 2 X YYYYmmdd as integers
# Output: days between the integers
sub calculate_distance {
    my($ptA, $ptB) = @_;
    ($ptA, $ptB) = ($ptB, $ptA) if ($ptA > $ptB);

return Delta_Days(map unpack('A4A2A2', $_), ($ptA, $ptB)) 
}

# past = the count value of the past
# future = the count value of the future
# past_distance = the distance from the present to the past
# future_distance = the distance from the present to the future
sub calculate_count { # where 'count' is the number of riders
    my($past, $future, $past_distance, $future_distance) = @_;

    my $total_change = $future - $past;
    my $total_distance = $future_distance + $past_distance;

    my $change_per_increment = $total_change / $total_distance;

return ($change_per_increment * $past_distance) + $past;
}

# Input: YYYY-mm-dd HH:00:00
# Output: ('YYYYmmdd', 'HH')
sub format_day_hour {
    my $datetime = shift;

 join('',
      map substr($datetime, $$_[0], $$_[1]),
	    	([0,4],[5,2],[8,2])), 		# YYYY-mm-dd
      substr($datetime, 11, 2)	      		# HH
}

# just loads the whole thing into RAM
sub fetch_data {
    my($train, $test);

    local $/ = undef;
    open(TRAIN, "../data/train.csv") or die "can't open: ../data/train.csv: $!";
      $train = <TRAIN>;
    open(TEST, "../data/test.csv") or die "can't open: ../data/test.csv: $!";
      $test = <TEST>;

return($train, $test)
}
