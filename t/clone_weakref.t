#!perl

use strict;
use warnings;
use Test::More tests => 4;
$| = 1;

BEGIN {
    chdir 't' if -d 't';
    unshift @INC, qw(../lib ../blib/lib);
    use_ok('Util');
}

BEGIN {
    use_ok('Scalar::Util', qw(weaken isweak));
}

SKIP: {
    skip ('SvWEAKREF not defined', 2) unless (&Util::weakref_is_defined());

    my $v1 = {};

    $v1->{a} = $v1;

    weaken ($v1->{a});

    my $v2 = Util::clone($v1);

    ok (isweak ($v1->{a}), 'original weakened ref');
    ok (isweak ($v2->{a}), 'cloned weakened ref');
}
