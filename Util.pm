package Util;

# Changelog
# chocolateboy: 2002-04-29: Added PIPE option to writeFile

use 5.006;
use strict;
use warnings;
use Errno;
use Carp qw(carp confess croak);
use POSIX qw(floor);

require Exporter;
require DynaLoader;
use AutoLoader 'AUTOLOAD';

our @ISA = qw(Exporter DynaLoader);

our %EXPORT_TAGS = (
    'io'	=> [ qw(readfile writefile appendfile reader mtime) ],
    'www'	=> [ qw(html jscript jshtml xmlparse urlize) ],
    'string'	=> [ qw(plural trim ltrim rtrim quote capitalize) ], 
    'misc'	=> [ qw(any uid respond clone) ],
    'math'	=> [ qw(div isnum isuv isbig isfloat isneg isinf isnan) ],
    'test'	=> [ qw(weakref_is_defined) ]);

our @EXPORT_OK = ( map { @$_ } values %EXPORT_TAGS );

our $VERSION = '0.01';

bootstrap Util $VERSION;

1;

__END__

=head1 NAME

    Util - Frequently Hacked Functions

=head1 DESCRIPTION

    A cookbook of tasty functions for the terminally lazy, impatient and hubristic

=head1 AUTHOR

    chocolateboy: chocolate.boy@email.com

=head1 SEE ALSO

    Scalar::Util, Clone

=head1 BUGS

    clone() currently segfaults if it encounters a Regex object

=head1 PUBLIC METHODS

=head1 any

usage:

    any ($arrayref)

	# or

    any ($hashref)

description:

    returns a randomly chosen member of the referenced array, or a random key
    from the referenced hashtable

=cut

sub any($; $) {
    my $ref = shift;
    my $top = shift;
    $ref = [ keys (%$ref) ] if (ref $ref eq 'HASH');
    croak ("any: invalid reference '$ref': expected ARRAY or HASH")
	unless (ref $ref eq 'ARRAY');
    $top = $#$ref + 1 unless (defined $top);
    return $ref->[floor(rand($top))];
}

=head1 isnum

usage:

    isnum ($val)

description:

    returns nonzero value (indicating the numeric type) if $val is a number

    The numeric types are a conjunction of the following flags:

    0x01    IS_NUMBER_IN_UV		    (number within UV range - maybe not int)
    0x02    IS_NUMBER_GREATER_THAN_UV_MAX   (the pointed-to UV is undefined)
    0x04    IS_NUMBER_NOT_INT		    (saw . or E notation)
    0x08    IS_NUMBER_NEG		    (leading minus sign)
    0x10    IS_NUMBER_INFINITY		    (this is big)
    0x20    IS_NUMBER_NAN		    (this is not)

    Rather than obliging the user to twiddle with bits, the following flavours of isnum
    (corresponding to the flags above) are also available:

    isuv
    isbig
    isfloat
    isneg
    isinf
    isnan

=cut

sub isnum ($) {
    my $val = shift;
    return is_num($val . ''); # stringify - ironically, looks_like_number always returns 1 unless arg is a string
}

sub isuv ($) {
    return (isnum(shift()) & 1) ? 1 : 0;
}

sub isbig ($) {
    return (isnum(shift()) & 2) ? 1 : 0;
}

sub isfloat ($) {
    return (isnum(shift()) & 4) ? 1 : 0;
}

sub isneg ($) {
    return (isnum(shift()) & 8) ? 1 : 0;
}

sub isinf ($) {
    return (isnum(shift()) & 16) ? 1 : 0;
}

sub isnan ($) {
    return (isnum(shift()) & 32) ? 1 : 0;
}

=head1 div

usage:

    my ($quotient, $remainder) = div ($numerator, $denominator);

    # e.g.
    
    my ($q, $r) = div (13, 3);

    # $q = 4, $r = 1:  13 ($numerator) = 4 ($quotient) x 3 ($denominator) + 1 ($remainder) 

description:

    integer division operator
    
    in list context, returns the quotient and remainder when the first operand ($numerator) is divided by the second ($denominator)
    
    i.e. 
    
	$numerator = $quotient * $denominator + $remainder

    in scalar context, returns just the quotient. To return the remainder, use %

=cut

sub div ($$) {
    my ($numerator, $denominator) = @_;
    my ($quotient, $remainder) = (int ($numerator / $denominator), $numerator % $denominator);
    return wantarray ? ($quotient, $remainder) : $quotient;
}

=head1 mtime

usage:

    mtime ($file)

description:

    returns the modification time of the specified file

=cut

sub mtime($) {
    return (stat($_[0]))[9];
}

=head1 respond

usage:

    respond ($scalar)
    respond (@list)

description:

    respond() performs a context-sensitive return.
    In void context the return value is printed
    In scalar context it is returned as a scalar
    In list context it is returned as a list

=cut

sub respond { # Context-sensitive return
    my @args = @_;
    my $arg = shift;
    my $context = (caller(1))[5];
    return @args if ($context);
    return $arg if (defined $context);
    print @args;
}

=head1 quote

usage:

    quote($str)

description:

    escapes all single quote ('), double-quote (") and vertical space
    (\n\r\f) characters with a backslash, visually flattening the resulting text

    useful for string export e.g. assigning a HTML page to a JavaScript var

    prints the escaped text out directly if called in void context

=cut

sub quote ($) { # Escape quotes and newlines
    my $escapee = shift;
    $escapee = '' unless (defined $escapee);
    $escapee =~ s/'/\\'/g;
    $escapee =~ s/"/\\"/g;
    $escapee =~ s/[\n\r\f]/\\n/g;
    respond ($escapee);
}

=head1 capitalize

usage:

    capitalize($str)

    # or 

    capitalize($str, @do_not_capitalize_these_words)

description:

    Initial-capitalizes $str i.e. any words (defined as consecutive characters between word-boundaries (\b))
    in $str that aren't already all caps are lowercased and their initials are uppercased.
    Note: apostrophes are treated as word characters, so "There's more than one way to do it" becomes
    "There's More Than One Way To Do It" rather than "There'S More Than One Way To Do It"

    Any arguments supplied after string are treated as exceptions and left as is

=cut

sub capitalize ($;@) {
   my $text = shift;
   if (@_) {
       local $_;
       my %exclude = map { $_ => 1 } @_;
       $text =~ s/([\w']+)/(($1 eq uc $1) || $exclude{$1}) ? $1 : "\u\L$1"/ge;
   } else {
       $text =~ s/([\w']+)/$1 eq uc $1 ? $1 : "\u\L$1"/ge;
   }
   return $text;
}

=head1 trim

usage:

    trim ($str)

description:

    removes whitespace from the beginning and end of $str and squashes
    multiple spaces down to single spaces

=cut

sub trim ($) {  # Aaaaaaaaaaaaaagh!!!
    my $spacey = shift;
    $spacey=~s/^\s+//;
    $spacey=~s/\s+$//;
    $spacey =~tr/ / /s;
    return $spacey;
}

=head1 ltrim

usage:

    ltrim ($str)

description:

    removes whitespace from beginning of $str

=cut

sub ltrim ($) {
    my $spacey = shift;
    $spacey=~s/^\s+//;
    return $spacey;
}

=head1 rtrim

usage:

    rtrim ($str)

description:

    removes whitespace from end of $str

=cut

sub rtrim ($) {
    my $spacey = shift;
    $spacey=~s/\s+$//;
    return $spacey;
}

=head1 uid

usage:

    uid()

description:

    returns a quick 'n' dirty Unique Identifier. Uses $$, so
    not necessarily reliable under SpeedyCGI, mod_perl &c.

=cut

sub uid () {
    srand ($$ | time); # FIXME: $$ isn't unique in SpeedyCGI mod_perl &c.
    return unpack "H*", pack "Nnn", time, $$, int (rand(60000));
}

=head1 html

usage:

    html ($text);

description:

    returns text with HTML Content-type header prefixed

    prints the prefixed page out directly if called in void context

=cut

sub html ($) {
    respond ("Content-type: text/html\n\n" . shift);
}

=head1 jscript

usage:

    jscript ($text);

description:

    returns the JavaScript in $text with the 'application/x-javascript' Content-type header prefixed

    prints the prefixed page out directly if called in void context

=cut

=head1 jshtml

usage:

    jshtml ($text);

description:

    returns the JavaScript in $text wrapped inside <script>, <head> and <html> tags.
    The resulting HTML is returned with a 'text/html' header.

    prints the result out directly if called in void context

=cut

sub jshtml ($) {
    respond (html(sprintf("<html><head><script><!--$/\t%s$///--></script></head><body></body></html>", shift())));
}

"Content-type: text/html\n\n<html><head><script>alert('hello, world!');</script></head></html>";

=head1 plural

usage:

    my $plural = plural($stem, $count);

	# or

    my $plural = plural($stem, $count, $plural);

description:

    plural() takes a singular word or word-stem as an argument;
    it evaluates $count to see if it is equal to 1;
    if it is, $stem is returned unchanged; otherwise $stem is
    pluralised by adding $plural, or 's' if $plural
    hasn't been supplied.

    thus:

	my $plural = plural('error', $error);

	will return:
	
	    'errors' if $error == 0
	    'error' if $error == 1
	    'errors' if $error > 1

    This simple implementation does not support irregular plurals that modify the stem.

=cut

sub plural ($$;$) {
    my ($string, $count, $plural) = @_;
    $plural = 's' unless (defined $plural);
    return ($count == 1) ? $string : $string . $plural;
}

=head1 urlize

usage:

	my $url = urlize('Foo: BAR baz'); # returns 'foo_bar_baz'

	# or

	my $url = urlize('Foo - BAR - baz', 'html'); # returns 'foo_bar_baz.html'

description:

	makes its text argument URL-friendly

	Returns the first argument lowercased with any consecutive non-alphanumeric characters replaced by an underscore.
	If the optional second argument is provided, this is appended as an extension prefixed by '.'
=cut

sub urlize($;$) {
    my $name = shift;
    croak ("urlize: name not defined") unless (defined $name);
    my $ext = scalar (@_) ? ".$_[0]" : '';
    $name =~ s/[^A-Za-z0-9]+/_/g; # replace consecutive non-alphanumeric characters with an underscore
    return lc($name) . $ext;
}

=head1 xmlparse

usage:

	xmlparse ($parser, $xml_path_or_data);

description:

	convenience wrapper for XML::Parser (or XML::Parser::Expat - or indeed any parser that supports parse() and parsefile())
	that is agnostic with regard to whether $xml is a file/filehandle or raw XML text.

	The $parser should be prefabricated according to taste.

=cut

sub xmlparse ($$) {
    my ($parser, $xml) = @_;
    my $file = '';
    my $parsed = 1;

    {
        local $SIG{__DIE__} = sub {
            my $err = shift;
            confess ("xmlparse: Can't parse $file: $err");
        };

        local $SIG{__WARN__} = sub {
            my $err = shift;
            carp ("xmlparse $file: $err");
        };

        if ($xml =~ /^\s*</) {
            $file = 'xml';
            $parser->parse($xml);
        } elsif (-s $xml) {
            $file = $xml;
            $parser->parsefile($xml);
        } else {
            $parsed = 0;
        }
    }

    croak ("can't find file: $xml: $!") unless ($parsed);
}

=head1 readfile

description:

    Swiss-Army Slurp

usage:

    # vanilla
    my $file = readfile($path);
	
    # logging
    my $file = readfile($path, LOG => $log);
	
    # lines
    my @file = readfile($path);
	
    # set Input Record Separator
    my @file = readfile($path, IRS => '...');
	
    # strip Input Record Separator from result
    my @file = readfile($path, IRS => '...', CHOMP => 1);
	
    # all together now...
    my @file = readfile($path, LOG => $log, IRS => '...', CHOMP => 1);
	
    # generator/continuation
    my $source = '';
    my $generator = readfile($path, IRS => '...', CHOMP => 1, GENERATOR => 1);

    while ($generator->(\$source)) {
	process($source);
    }

	# or

    while ($generator->()) { # implied target: $_
	process($_);
    }
	
    # synonyms for IRS => DELIM, SPLIT, DELIMITER
	
=cut

# die and warn handlers for reader: saves allocating a closure every time
sub _die  { die('readfile: ' . shift) }
sub _warn { die('readfile: ' . shift) }

sub readfile($;%) { # this prototype doesn't do what it says on the tin
    my ($file, %args) = @_;
    goto &reader if $args{GENERATOR};
    local $_;
    my ($die, $warn, $chomp) = @args{qw(DIE WARN CHOMP)};
    # tread carefully in case the delimiter is '' or 0
    my $delim;

    for my $irs (qw(IRS DELIM SPLIT DELIMITER)) {
	next unless (exists $args{$irs});
	$delim = $args{$irs};
	last;
    }

    $die ||= \&_die;
    $warn ||= \&_warn;

    $die->("filename not defined") unless (defined $file);

    my @lines = ();

    local (*FILE);
    if (open(FILE, $file)) {
	local $/;

	if (flock(FILE, 1)) { # LOCK_SH
	    $/ = $delim if (defined $delim);
	    while (<FILE>) {
		chomp if ($chomp);
		push @lines, $_; 
	    }
	    # FIXME: release shared lock?
	} else {
	    close FILE;
	    $die->("Can't lock <$file>: $!") 
	}
    } else {
	$die->("Can't open <$file>: $!");
    }

    $warn->("Can't close <$file>: $!") unless (close FILE);

    return wantarray ? @lines : join('', @lines);
}

=head1 reader

    This method implements a generator/continuation interface
    to the fine art of file slurpage. Essentially, it provides a private
    lexically scoped filehandle and an associated file reader
    (in the form of a closure).

    reader() is also accessible via readfile().

    If readfile() is called with a GENERATOR => 1 argument pair,
    then a closure is returned.
    This closure should be called with a reference to the variable
    one wishes to be assigned the next line from the file.
    If no argument is supplied then $_ is assumed
    to be the target.

    The generator yields true while the file is slurping, and undef thereafter.

    For more information on the relative performance of fileHandle v local (*FH)
    (local is used in 'vanilla' readfile()) vide:

	http://groups.google.com/groups?
	    hl=en&frame=right&th=f6035f6588fa7bfe&seekm=tbaf8dq702.fsf%40blue.sea.net#s

    Unfortunately, local() won't work in reader()
    (as a way of creating anonymous, temporary filehandles)
    because close() is called automatically on such filehandles
    at the end of the scope. The end of the scope
    means the end of the call to reader() or the end of a (single)
    call to $generator->() - neither of which
    correspond to the end of days for the filehandle as far as the user is concerned.

    What's needed is a solution similar to Alexander Alexandrescu's
    ScopeGuard, and that's what is implemented here:

	http://www.cuj.com/experts/1812/alexandr.htm?topic=experts

=cut

sub reader ($;%) {
    my ($file, %args) = @_;

    # print "read_files: ", $/, Dumper({ @_ }), $/ if (scalar @_);
    my ($die, $warn, $chomp) = @args{qw(DIE WARN CHOMP)};

    $die ||= \&_die;
    $warn ||= \&_warn;

    $die->("filename not defined") unless (defined $file);

    # Copy $/ before local() obliterates it below
    my $delim = $/;

    # Now $/ is safe, we can override it if necessary
    # support aliases for the delimiter
    # tread carefully in case the delimiter is '' or 0
    for my $irs (qw(IRS DELIM SPLIT DELIMITER)) {
	next unless (exists $args{$irs});
	$delim = $args{$irs};
	last;
    }

    # $sg satisfies 3 requirements
    #	1. it's a lexical *reference* - needs to be a ref to give us a number
    #	2. it's referred to within the body of generator - a variable isn't
    #	   bound into a closure unless it's referred to
    #	3. it's private - the user is shielded from the implementation

    my $sg = Scope::Guard->new();
    my $handle = 'FH_' . int($sg);
	    
    {
	no strict 'refs';
	$die->("can't open <$file>: $!") unless (open($handle, $file));
	$handle = *$handle; # change $handle from a string into a filehandle
    }

    # wait until $handle is a filehandle before we guard it
    $sg->guard($handle, \&myclose);

    unless (flock($handle, 1)) { # LOCK_SH
	close $handle;
	$die->("Can't lock <$file>: $!");
    }

    # arg is optional but if supplied must be (or must be coercable into) a scalar ref
    my $generator = sub(;\$) {
	return unless ($sg);
	local $/ = $delim; # ambient locals aren't (apparently) bound into closures
	my $yield = $_[0] ? shift : \$_;

	if (defined ($$yield = readline($handle))) {
	    chomp ($$yield) if ($chomp);
	    return 1;
	} else {
	    # FIXME: release shared lock?
	    return ($sg = undef);
	}
    };

    return $generator;
}

=head1 writefile

usage:

    writefile($file, $data, %args);

description:

    Write $data to filename $file. In theory, additional herbs
    and spices are specified as a list of pairs,
    in the same manner as readfile (and reader).
    In practice, only APPEND => 1 for append (as opposed to truncate)
    and/or 'MODE' => $mode (to roll your own file access mode) are currently defined.

=cut

sub writefile ($$;%) {
    my ($file, $data, %args) = @_;
    my $log = $args{LOG};
    my ($die, $warn) =
	$log ?
	(sub { $log->fatal(shift) }, sub { $log->warning(shift) }) :
	(sub { die('writefile: ' . shift) }, sub { warn('writefile: ' . shift) });

    my $mode = (exists $args{MODE}) ? $args{MODE} : ($args{APPEND} ? '>>' : '>');
    local (*FILE);
    # print STDERR "opening $mode$file\n";
    if (open(FILE, "$mode$file")) {
	if (flock(FILE, 2)) { # LOCK_EX
	    print FILE $data;
	} else {
	    close FILE;
	    $die->("Can't lock <$file>: $!");
	}
    } else {
	$die->("Can't open <$file>: $!");
    }

    # This is strictly unneccessary, as local (*FOO) takes care of
    # closing the file automatically at end-of-scope 
    # But we'll use that as a lifesaver rather than a way of life
    $warn->("Can't close <$file>: $!") unless (close FILE);
}

=head1 appendfile

usage:

    appendfile($file, $data, %args);

description:

    This is a simple wrapper for writefile()'s APPEND option.
    Appends $data to the file whose path is specified by $file.

    See writefile() for %args options.

=cut

sub appendfile ($$;%) {
    push @_, APPEND => 1;
    goto &writefile;
}

=head1 clone

usage:

    use Util qw(clone);

    $a = Foo->new();
    $b = { foo => 'bar', move => 'zig' };

    $c = clone($a);
    $d = clone($b);

    # or

    my $node4 = {
	name	    => 'node4',
	attr	    => 'foo',
	children    => [ $node5, $node6 ],
	parent	    => $node3
    };

    # clone $node4 but preserve the original $node3 (rather than cloning
    # through it all the way to the root)

    my $clone = clone($node4, [ $node3 ]);

	# or, equivalently

    my $clone = clone($node4, [ $node4->{parent} ]);

description:

    clone() returns a recursive copy of its argument, which can be an
    arbitrary (scalar) type including nested hash, array and reference types
    (including weak references), tied variables and objects.

    To duplicate non-scalar types (e.g. lists, arrays and hashes), pass them
    in by reference. e.g.
	
	my $copy = clone (\@array);

	# or

	my %copy = %{ clone (\%hash) };

    clone() takes an optional second argument: a reference to an array
    containing a list of exceptions i.e. values that should be 'passed-thru'
    verbatim. This is useful for, amongst other things, cloning nodes in a
    hierarchy without duplicating the structure all the way to the root.

    For a slower, but more flexible solution see Storable's dclone().

=head1 PRIVATE METHODS

=head1 myclose

    Used as an auxiliary filehandle destructor by reader()

=cut

# FIXME: could inherit warner to flag any close() problems

sub myclose {
    my $handle = shift;
    close $handle;
}

package Scope::Guard;

=head1 Scope::Guard

=head1 DESCRIPTION

    Confer lexical semantics on an arbitrary resource

=head1 METHODS

=head1 new

usage:

    my $sg = Scope::Guard->new();

description:

    Creates a new ScopeGuard object. ScopeGuard provides resource
    management for a non-lexically-scoped variable
    by wrapping that variable in a lexical whose destructor then
    manages the bound resource.

    Thus the lifetime of a non-lexical resource can be made
    commensurate with that of a blessed lexical.

    In other words, a resource that's messy, painful or
    inconvenient to close/free/cleanup can be 'automagically' managed
    as painlessly as any temporary. Forget about it, let it go out of
    scope, or set it to undef and resource
    management kicks in via the ScopeGuard destructor (DESTROY, of course)
    which feeds its second member (handler) its first member (resource).

    In addition to this resource management functionality,
    the ScopeGuard pointer value (as an integer) is used to
    create a unique filehandle *name* within Util::reader()
    (called by Util::readfile). In practice, any lexical reference
    could have been used to provide a safe filehandle name.
    The ScopeGuard object just happened to be the most convenient
    lexical at our disposal.

    For more information on ScopeGuard, vide:

	http://www.cuj.com/experts/1812/alexandr.htm?topic=experts


=cut

sub new {
    my ($class, $resource, $handler) = @_;
    my $self = [ $resource, $handler ];
    bless $self, ref $class || $class;
}

=head1 guard

usage:

    $sg->guard($resource, $handler);

description:

    Initialize a ScopeGuard object with the resource it should
    manage and the handler that should be called to implement
    that management when the ScopeGuard object's destructor is called.

=cut

sub guard {
    my $self = shift;
    @$self = @_;
}

=head1 DESTROY

usage:

    $sg->DESTROY();

description:

    Not called directly. The destructor is a thin wrapper around
    the invocation of the handler on the resource

=cut

sub DESTROY {
    my $self = shift;
    my ($resource, $handler) = @$self;
    # print "destroying $resource", $/;
    $handler->($resource);
}

1;
