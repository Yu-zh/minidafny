#!/usr/bin/env perl

use v5.10;
use strict;
use warnings;
use File::Find;
use File::Copy;
use Cwd 'abs_path';
use threads;

our %PROJ_INFO;
our %TEST_QUEUE;

&init_check();
&cmd_parse();
my $compilation_success = &compile();
if($compilation_success)
{
    &run();
}

####################################################################################################

# Misc.

####################################################################################################

sub cmd_parse()
{
    foreach my $current_arg (@ARGV)
    {

    }
}
sub compile()
{
    my @src_filelist = glob "$PROJ_INFO{'SRC_DIR'}/*.scala";
    foreach my $src_file (@src_filelist)
    {
        say "[info] src file $src_file";
    }
    return !(system "scalac -d $PROJ_INFO{'RESULT_DIR'} @src_filelist");
}
sub run()
{
    say "run";
}

sub init_check()
{
    $PROJ_INFO{'PROJ_DIR'}         = &get_script_path();
    $PROJ_INFO{'SRC_DIR'}          = "$PROJ_INFO{'PROJ_DIR'}/Src";
    $PROJ_INFO{'BENCH_DIR'}        = "$PROJ_INFO{'PROJ_DIR'}/Benchmarks";
    $PROJ_INFO{'RESULT_DIR'}       = "$PROJ_INFO{'PROJ_DIR'}/_Results";
    $PROJ_INFO{'BIN'}              = "$PROJ_INFO{'RESULT_DIR'}/minidafny";

    foreach my $path (keys %PROJ_INFO)
    {
        if($path =~ 'DIR')
        {
            mkdir $PROJ_INFO{'RESULT_DIR'} if !-e $PROJ_INFO{'RESULT_DIR'};
            die "[error] unable to create dir at $PROJ_INFO{'RESULT_DIR'}" if !-e $PROJ_INFO{'RESULT_DIR'};

            die "[error] Wrong path for $path at $PROJ_INFO{$path}" if !-e $PROJ_INFO{$path};
        }
    }
}

sub get_script_path
{
    (my $final_path = $+{path}) =~ s/\/$// if(abs_path($0) =~ /(?<path>\/.+\/)/);
    return $final_path;
}