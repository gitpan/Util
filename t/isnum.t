# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..22\n"; }
END {print "not ok 1\n" unless $loaded;}

use Util qw(:math);
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

# Insert your test code below (better if it prints "ok 13"
# (correspondingly "not ok 13") depending on the success of chunk 13
# of the test code):

use strict;
use warnings;

my $ok = 2;

$|=1;

sub ok {
    my ($result, $test) = @_;
    print (($result == $test) ? "ok $ok\n" : "not ok $ok ($test)\n");
    $ok++;
}

ok (1, isnum(0));		# 2
ok (5, isnum('0.00'));		# 3
ok (0, isnum(undef));		# 4
ok (0, isnum('A'));		# 5
ok (0, isnum('A0'));		# 6
ok (0, isnum('0A'));		# 7
ok (0, isnum(\&ok));		# 8
ok (1, isuv((2 ** 32) - 1));	# 9
ok (1, isuv((2 ** 32) - 1));	# 10
ok (1, isbig((2 ** 32)));	# 11
ok (0, isbig(2));		# 12
ok (1, isfloat(3.1415927));	# 13
ok (0, isfloat(3));		# 14
ok (1, isneg(-1));		# 15
ok (1, isneg(-3.1415927));	# 16
ok (0, isneg(1));		# 17
ok (0, isneg(3.1415927));	# 18
ok (1, isinf('Inf'));		# 19
ok (0, isinf(3.1415927));	# 20
ok (1, isnan('nan'));		# 21
ok (0, isnan(42));		# 22
