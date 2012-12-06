#!/usr/bin/perl
#
# This script is designed to have an email piped to it eg. from mutt.
# It will split apart all the text/calendar attachments and enter them into
# the 'remind' calendar.
#

use strict;
use warnings;

use MIME::Parser;

my $CONVERT = '~/.mutt/ical2rem.pl';
my $REMINDERS = '~/remind/mutt.rem';

################################################################################

my $parser = new MIME::Parser;
$parser->output_under('/tmp');
my $entity = $parser->parse(\*STDIN);

my @parts = $entity->parts();
my $count = 0;

foreach my $part (@parts) {
  if ($part->head->mime_type eq 'text/calendar') {
    my $body = $part->bodyhandle;
    my $cmd = $CONVERT.' '.$body->path.' >> '.$REMINDERS;
    print STDERR `$cmd`;
    last if ($? != 0);
    $count++;
  }
}

$parser->filer->purge;
if ($count == 0) {
  print STDERR "No calendar entries found.";
  exit(1);
}

exit(0);
