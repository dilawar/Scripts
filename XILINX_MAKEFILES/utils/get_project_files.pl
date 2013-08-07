#
# Output a list of the files found in the .prj file of a Xilinx WebPACK/ISE project
#
open(PRJFILE,$ARGV[0]) || die $!;
while(<PRJFILE>) {
  @fields = split(/\s+/,$_);
  $f = pop(@fields);  # file name is last field of each line
  $f =~ s/\"//g;  # remove any quotations around file names
  print " " . $f;
}

