# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..19\n"; }
END {print "not ok 1\n" unless $loaded;}

use Util qw(clone);
$loaded = 1;
print "ok 1\n";

######################### End of black magic.

use strict;
use warnings;

use Tie::IxHash;

require "debug.pl";

$|=1;

my $ok = 2;

my $passthru = { 'alpha' => 'beta', 'gamma' => 'vlissides' };
my $exclude = [];

my $original = Zig->new();
$original->{'move'} = 'zig';
$original->{'int'} = 42;
$original->{'double'} = 3.1415;
$original->{'array'} = [ 'make', 'your', 'time' ];
$original->{'self'} = $original;
$original->{'code'} = \&test;
$original->{'passthru'} = $passthru;
$original->{'exclude'} = $exclude;
# FIXME: regex type causes segfault: newSVsv returns the original RV for some reason
# $original->{'regex'} = qr/Hello World/i;

my $clone = clone($original);
my $exception = clone($original, [ $passthru, $exclude ]);

runtests();

exit(0);

######################### subs and package

sub ok {
    my ($result, $test) = @_;
    print (($result) ? "ok $ok\n" : "not ok $ok ($test)\n");
    $ok++;
}

sub test {
    return "all your base are belong to us";
}

sub test_tie {
    my %hash = ();
    my $ok = 0;
    my $t = tie(%hash, 'Tie::IxHash');
    my @keys = qw(foo bar foobar barfoo alpha beta gamma delta epsilon);

    for my $index (0..9) {
	my $i = int(rand(scalar @keys));
	my $j = int(rand(scalar @keys));
	my $key = (rand > .5) ? $keys[$i] . $index . $keys[$j] :
	    $index . $keys[$i] . $index;
	$hash{$key} = $index;
    }

    my $hash = \%hash;
    my $copy = clone($hash);
    ok (($hash != $copy), "tied hash new SV");
    ok (((join '', (keys %hash)) eq
	(join '', keys %$copy)), "tied hash keys");
    ok (((join '', (values %hash)) eq
	(join '', values %$copy)), "tied hash values");
    %$copy = ();
    $copy->{'foo'} = 3;
    $copy->{'bar'} = '.';
    $copy->{'foobar'} = 1;
    $copy->{'move'} = 4;
    $copy->{'zig'} = 1;
    $copy->{'alpha'} = 5;
    $copy->{'beta'} = 9;
    $copy->{'gamma'} = 2;
    $copy->{'vlissides'} = 7;
    ok (((join '', (values %$copy)) == 3.1415927), "tied hash semantics");
}

sub runtests {
    ok(!(defined (clone (undef))), "undef");
    ok($clone, "basic");
    ok(($clone->{'array'} ne $original->{'array'}) &&
	($clone ne $original), "new SV");
    ok((ref $original) eq (ref $clone), "same reftype");
    ok(($clone->{'move'} eq $original->{'move'}), "PV");
    ok(($clone->{'int'} == $original->{'int'}), "IV");
    ok(($clone->{'double'} == $original->{'double'}), "NV");
    ok((join('', @{$clone->{'array'}}) eq 
	join('', @{$original->{'array'}})), "AV");
    ok(($clone->{'self'} eq $clone), "self reference");
    ok(($clone->{'code'} == $original->{'code'}), "CV is unchanged");
    ok(($clone->{'code'}->() eq test()), "CV works");
    ok(($clone->move() eq test()), "method works");
    ok(($exception->{'passthru'} ne $clone->{'passthru'}) &&
	($exception->{'passthru'} eq $passthru), "passthru");
    ok(($exception->{'exclude'} ne $clone->{'exclude'}) &&
	($exception->{'exclude'} eq $exclude), "passthru multiple args");
    # FIXME: regex type causes segfault: newSVsv returns the original RV for some reason
    # ok((ref $clone->{'regex'} eq 'Regex'), "regex");
    # ok(($clone->{'regex'} eq $original->{'regex'}), "regex");
    test_tie();
}

package Zig;

sub new {
    my $class = shift;
    bless {}, ref $class || $class;
}

sub move {
    my $self = shift;
    return (join ' ',
	('all',
	$self->{'array'}->[1],
	'base',
	'are',
	'belong',
	'to',
	'us'));
}
