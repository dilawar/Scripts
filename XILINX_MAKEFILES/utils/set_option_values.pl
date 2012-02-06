#
# Set option values in an options file of a Xilinx WebPACK/ISE project
#
$option_file = shift @ARGV;    # first argument is the option file name
open(FILE,$option_file) || die $!;
@options = <FILE>;             # read in all the options
$options = join("",@options);  # join all options into one big string

# remaining arguments are new option strings that will replace existing options
foreach (@ARGV) {
  @option_fields = split(/\s+/,$_);  # split new option string into fields
  $option_value = pop @option_fields; # new option value is last field in the string
  $option_name = join(" ",@option_fields);  # option name is all the preceding fields
  $options =~ s/$option_name\s+.*/$option_name $option_value/gi;  # replace existing value with new value
}

print $options;  # print the updated option file

