#!/usr/bin/env perl

# ------------------------------------------------------------------------------
use Modern::Perl;
use Try::Catch;
use File::Copy;
use experimental qw/switch/;
use asmtidy;

# ------------------------------------------------------------------------------
my $indent_left         = 4;
my $indent_comma        = 0;
my $indent_tail_comment = 1;
my $indent_operands     = 1;
my $unaligned_comments  = q{-};
my $del_empty_lines     = 'no';
my $user_names          = q{};
my $output              = undef;
my $bak                 = 'orig';
my $at                  = asmtidy->new();
my $file                = shift @ARGV;
_usage('No input file!') unless $file;

while (@ARGV) {
    given ( my $key = shift @ARGV ) {

        my $val = shift @ARGV;
        when ('-bak') {
            _noval($key) unless $val;
            $bak = $val;
        }
        when ('-del') {
            _noval($key)  unless $val;
            _badval($key) unless $val =~ /^yes|no|all$/;
            $del_empty_lines = $val;
        }
        when ('-io') {
            _noval($key)  unless $val;
            _badval($key) unless $val =~ /^\d+|tab\d+$/;
            $indent_operands = $val;
        }
        when ('-itc') {
            _noval($key)  unless $val;
            _badval($key) unless $val =~ /^\d+|tab\d+$/;
            $indent_tail_comment = $val;
        }
        when ('-lm') {
            _noval($key)  unless defined $val;
            _badval($key) unless $val =~ /^\d+|tab\d+$/;
            $indent_left = $val;
        }
        when ('-sc') {
            _noval($key)  unless defined $val;
            _badval($key) unless $val =~ /^\d+$/;
            $indent_comma = $val;
        }
        when ('-o') {
            _noval($key) unless $val;
            $output = $val;
        }
        when ('-slc') {
            _noval($key)  unless $val;
            _badval($key) unless $val =~ /^left|right+$/;
            $unaligned_comments = $val;
        }
        when ('-un') {
            _noval($key) unless defined $val;
            $user_names .= q{,} if $user_names;
            $user_names .= $val;
        }
        default {
            _usage("Invalid key \"$key\"!");
        }
    }
}

$at->set_opt(
    {   'del_empty_lines'     => $del_empty_lines,
        'indent_comma'        => $indent_comma,
        'indent_left'         => $indent_left,
        'indent_operands'     => $indent_operands,
        'indent_tail_comment' => $indent_tail_comment,
        'unaligned_comments'  => $unaligned_comments,
        'user_names'          => $user_names,
    }
);

# ------------------------------------------------------------------------------
my $rc;
try {
    if ( $file eq q{-} ) {
        my $olds = $/;
        local $/ = undef;
        my @stdin = <>;
        $rc = $at->tidy_content( \@stdin );
    }
    else {
        $rc = $at->tidy_file($file);
    }
}
catch {
    say $at->name() . " error: $_!";
    exit -2;
};

if ( $output && $output ne q{-} ) {

    $output = $file if $output eq q{+};
    _create_bak_file($output);
    if ( open my $f, '>:encoding(utf8)', $output ) {
        print {$f} $rc;
        close $f;
    }
    else {
        say "Can not write \"output\": $!";
        exit -4;
    }
}
else {
    print $rc;
}

# ------------------------------------------------------------------------------
sub _usage
{
    my ($msg) = @_;

    my $program = $0;
    $program =~ s{^.*/([^/]+$)}{$1};

    say STDERR $at->id();
    say STDERR "\n$msg" if $msg;

    say STDERR qq{
Usage: $program {file|-} [options]. Valid options are:

  -bak extension
    Extension for backup file (default: orig)
    
  -del {yes|no}
    Delete empty lines (yes - leave only one, default: no)
    
  -io {N|tabN}
    Indent second operands; number of spaces (defaut, 1 space) or TAB's
    filled up to N spaces 
  
  -itc N
    Indent tail comments (default: 1 space)

  -lm N
    Left margin (default: 4 spaces)  
 
  -o file
    Output to file (default or -: stdout, +: rewrite source)
    
  -sc N
    Spaces between comma and second operand (default: 0)
    
  -slc {left|right}
    Move single line comments to left margin, or to next line begin 
    (default: leave unchanged)
    
  -un {name1[,name2;...]}
    List of user names (separators: , or ;)

Examples:

  # read file.pl from stdin and output result to stdout:
  cat file.pl | $program -o - 
  # or
  cat file.pl | $program 

  # read file.pl and rewrite it by result:
  $program file.pl -o + 

  See in action online at http://ato.su/asmtidy/
};
    exit -1;
}

# ------------------------------------------------------------------------------
sub _noval
{
    my ($key) = @_;
    return _usage("No value for key \"$key\"!");
}

# ------------------------------------------------------------------------------
sub _badval
{
    my ($key) = @_;
    return _usage("Invalid value for key \"$key\"!");
}

# ------------------------------------------------------------------------------
sub _create_bak_file
{

    my $bakfile = $file;
    while ( -f $bakfile ) {
        $bakfile = "$bakfile.$bak";
    }
    unless ( copy( $file, $bakfile ) ) {
        say "Can not create backup file \"$bakfile\": $!";
        exit -3;
    }
}

# ------------------------------------------------------------------------------
# That's All, Folks!
# ------------------------------------------------------------------------------

