#!/usr/bin/perl

#$input=$ARGV[0];
#$output_h=$2.h;
#$output_c=$2.c;

if ($#ARGV < 0) {
	 print "uso $EXECUTABLE_NAME input.h \n";
	 exit 1;
}

$input_h=$ARGV[0];
$input_h=~/.*\/(.*)/;
$input_h_filename=$1;

$output_name=$input_h_filename;
$output_name=~ s/lfI(.*).h/lf$1/;;

open(INPUT, $input_h) || die "Impossible to open $input_h";

open(OUTPUT, ">$output_name.hpp") || die "can't open $output_name.h\n";;

print OUTPUT "#ifndef _".uc($output_name)."_H\n";
print OUTPUT "#define _".uc($output_name)."_H\n\n";
print OUTPUT "#include \"$input_h_filename\"\n\n";

$printing=0;
while ($l=<INPUT>) {
	 
	 if ($l =~ /\/\* Implementation file \*\//) {
		  goto ALGO;
	 }
	 if ($printing == 1) {
		  print OUTPUT $l;
	 }
	 if ($l =~ /\/\* Header file \*\//) {
		  $printing=1;
	 }
}
ALGO:
print OUTPUT "#endif\n";

close(OUTPUT);

open(OUTPUT, ">$output_name.cpp") || die "can't open $output_name.cpp\n";

print OUTPUT "#include \"$output_name.hpp\"\n\n";

while ($l=<INPUT>) {
	 if ($l =~ /\/\* End of implementation class template\. \*\//) {
		  $printing=0;
	 }
	 if ($printing == 1) {
		  print OUTPUT $l;
	 }
}

close(OUTPUT);

close(INPUT);
