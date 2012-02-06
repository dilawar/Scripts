#
# Get selected option value from an options file of a Xilinx WebPACK/ISE project
#
$option_file = shift @ARGV;         # first argument is the option file name
open(FILE,$option_file) || die $!;
$option_name = $ARGV[0];            # second argument is the option name to search for

# read lines from option file looking for the given option name
while(<FILE>) {
  chop;
  /^$option_name\s+/ && print $';   # print out the value for the given option
}

