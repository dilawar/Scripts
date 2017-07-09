#!/usr/bin/perl -w

use strict;

open my $fh, '-|', 'compiler', @_ or die $!;

my $last_line = <$fh> // exit;
while (defined(my $line = <$fh>)) {
    my($file, $l, $c) = $line =~ /^    at (.+?):(\d+):(\d+)$/;
    print "$file:$l:$c: $last_line" if defined($file);
    $last_line = $line;
}
