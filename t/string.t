#!/usr/local/bin/perl

use strict;
use warnings;

use Data::Dumper; $Data::Dumper::Terse = $Data::Dumper::Indent = 1;

use Test::More tests => 12;

BEGIN {
    chdir 't' if (-d 't');;
    unshift @INC, qw(../lib ../blib/lib);
    use_ok('Util', qw(:string)); # 1
}

=pod
our %EXPORT_TAGS = (
    'io'        => [ qw(readfile writefile appendfile reader atime mtime ctime) ],
    'www'       => [ qw(html jscript jshtml xmlparse urlize redirect) ],
    'string'    => [ qw(plural trim ltrim rtrim quote capitalize commify) ],
    'misc'      => [ qw(any id respond clone) ],
    'math'      => [ qw(div isnum isuv isbig isfloat isneg isinf isnan inf infinity) ],
    'test'      => [ qw(weakref_is_defined) ]);
=cut


my $commify = '';

is (plural('cat', 0), 'cats', 'plural: 0 cats');
is (plural('cat', 1), 'cat', 'plural: 1 cat');
is (plural('cat', 2), 'cats', 'plural: 2 cats');
is (plural('ox', 0, 'en'), 'oxen', 'plural: 0 oxen');
is (plural('ox', 1, 'en'), 'ox', 'plural: 1 ox');
is (plural('ox', 2, 'en'), 'oxen', 'plural: 2 oxen');
is (capitalize("salem's lot: UPPER lower Ucfirst iPod nVidia CeBIT"),
    "Salem's Lot: UPPER Lower Ucfirst iPod nVidia CeBIT", 'capitalize');
is(ltrim(' trim  test '), 'trim  test ', 'ltrim');
is(rtrim(' trim  test '), ' trim  test', 'rtrim');
is(trim(' trim  test '), 'trim test', 'trim');

$commify .= sprintf "    %-9u %11s%s", $_, commify($_), $/
    for (map { $_ x $_ } 1 .. 9);

is ($commify, <<EOS, 'commify');
    1                   1
    22                 22
    333               333
    4444            4,444
    55555          55,555
    666666        666,666
    7777777     7,777,777
    88888888   88,888,888
    999999999 999,999,999
EOS

