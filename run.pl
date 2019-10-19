#!/usr/bin/env perl

use v5.10;
use strict;
use warnings;
use File::Find;
use File::Copy;
use Cwd 'abs_path';
use threads;

our %PROJ_INFO;
our %BENCH_INFO;
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
        if($current_arg eq '-clean')
        {
            say "[info] cleaning $PROJ_INFO{'RESULT_DIR'} ...";
            `rm -irf $PROJ_INFO{'RESULT_DIR'}/*`;

            say "[info] cleaning $PROJ_INFO{'OBJ_DIR'} ...";
            `rm -irf $PROJ_INFO{'OBJ_DIR'}/*`;
        }
        elsif($current_arg eq '-all')
        {
            foreach my $test_name (keys %BENCH_INFO)
            {
                $TEST_QUEUE{$test_name}{'path'}   = $BENCH_INFO{$test_name}{'path'};
                $TEST_QUEUE{$test_name}{'valid'}  = $BENCH_INFO{$test_name}{'valid'};
                $TEST_QUEUE{$test_name}{'result'} = 'pending';
            }
        }
    }
}
sub compile()
{
    my @src_filelist = glob "$PROJ_INFO{'SRC_DIR'}/*.scala";
    foreach my $src_file (@src_filelist)
    {
        say "[info] found src file $src_file";
    }
    return !(system "scalac -d $PROJ_INFO{'OBJ_DIR'} @src_filelist");
}
sub run()
{
    foreach my $test_name (keys %TEST_QUEUE)
    {
        print "[info] running $test_name ... ";
        `scala -cp $PROJ_INFO{'OBJ_DIR'} $PROJ_INFO{'BIN'} $TEST_QUEUE{$test_name}{'path'} > $PROJ_INFO{'RESULT_DIR'}/$test_name.vc`;
        print "ok\n";
    }
}

sub init_check()
{
    $PROJ_INFO{'PROJ_DIR'}         = &get_script_path();
    $PROJ_INFO{'SRC_DIR'}          = "$PROJ_INFO{'PROJ_DIR'}/Src";
    $PROJ_INFO{'BENCH_DIR'}        = "$PROJ_INFO{'PROJ_DIR'}/Benchmarks";
    $PROJ_INFO{'OBJ_DIR'}          = "$PROJ_INFO{'PROJ_DIR'}/_Objs";
    $PROJ_INFO{'RESULT_DIR'}       = "$PROJ_INFO{'PROJ_DIR'}/_Results";
    $PROJ_INFO{'BIN'}              = "VCGen";

    foreach my $path (keys %PROJ_INFO)
    {
        if($path =~ 'DIR')
        {
            &mkdir_die($PROJ_INFO{'RESULT_DIR'});
            &mkdir_die($PROJ_INFO{'OBJ_DIR'});

            die "[error] Wrong path for $path at $PROJ_INFO{$path}" if !-e $PROJ_INFO{$path};
        }
    }

    my @bench_filelist = glob "$PROJ_INFO{'BENCH_DIR'}/*/*.imp";
    foreach my $file (@bench_filelist)
    {
        if($file =~ /\/.+\/(?<name>.+)\.imp$/)
        {
            my $name      = $+{name};
            my $validness = !($file =~ 'invalid') ? 1 : 0;
            $BENCH_INFO{$name}{'path'}  = $file;
            $BENCH_INFO{$name}{'valid'} = $validness;
            say "[info] found benchmark file $file, expected result is " . ($validness ? "valid" : "invalid");
        }
    }   
}

sub get_script_path
{
    (my $final_path = $+{path}) =~ s/\/$// if(abs_path($0) =~ /(?<path>\/.+\/)/);
    return $final_path;
}

sub mkdir_die
{
    my $dirpath = @_;
    
    mkdir dirpath if !-e dirpath;
    die "[error] unable to create dir at dirpath" if !-e dirpath;
}
