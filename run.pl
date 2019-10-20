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
our $INFO_LEVEL = 0;

&init_check();
&cmd_parse();
my $compilation_success = &compile();
if($compilation_success)
{
    &run();
    &report();
}

####################################################################################################

# Main.

####################################################################################################

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

            #&info_print(5, "found benchmark file $file, type is $validness");
        }
    }
}

sub cmd_parse()
{
    my $need_clean = 0;
    foreach my $current_arg (@ARGV)
    {
        if($current_arg eq '-clean')
        {
            $need_clean = 1;
        }
        elsif($current_arg =~ /-info=(?<level>\d)/)
        {
            $INFO_LEVEL = $+{level};
        }
        elsif($current_arg eq '-all')
        {
            foreach my $test_name (keys %BENCH_INFO)
            {
                $TEST_QUEUE{$test_name}{'path'}   = $BENCH_INFO{$test_name}{'path'};
                $TEST_QUEUE{$test_name}{'valid'}  = $BENCH_INFO{$test_name}{'valid'};
            }
        }
        elsif($current_arg =~ /.*\/?(?<name>.+)\.imp$/)
        {
            $TEST_QUEUE{$+{name}}{'path'}   = $current_arg;
            $TEST_QUEUE{$+{name}}{'valid'}  = 1;
        }
        else
        {
            die "unregconized parameter $current_arg";
        }
    }

    if($need_clean)
    {
        &info_print(5, "cleaning $PROJ_INFO{'RESULT_DIR'} ...");
        `rm -irf $PROJ_INFO{'RESULT_DIR'}/*`;

        &info_print(5, "cleaning $PROJ_INFO{'OBJ_DIR'} ...");
        `rm -irf $PROJ_INFO{'OBJ_DIR'}/*`;
    }

    foreach my $test_name (keys %BENCH_INFO)
    {
        &info_print(5, "found benchmark $test_name at location $BENCH_INFO{$test_name}{'path'}");
    }
}

sub compile()
{
    my @src_filelist = glob "$PROJ_INFO{'SRC_DIR'}/*.scala";
    foreach my $src_file (@src_filelist)
    {
        &info_print(5, "found src file $src_file");
    }
    &info_print(0, '') if($INFO_LEVEL == 5);
    return !(system "scalac -d $PROJ_INFO{'OBJ_DIR'} @src_filelist");
}
sub run()
{
    foreach my $test_name (keys %TEST_QUEUE)
    {
        &info_print(5, "running $test_name ... ");
        `scala -cp $PROJ_INFO{'OBJ_DIR'} $PROJ_INFO{'BIN'} $TEST_QUEUE{$test_name}{'path'} > $PROJ_INFO{'RESULT_DIR'}/$test_name.vc`;
    }
    &info_print(0, '') if($INFO_LEVEL == 5);
}

sub report()
{
    foreach my $test_name (keys %TEST_QUEUE)
    {
        &info_print(5, "checking $test_name with validness =  $TEST_QUEUE{$test_name}{'valid'}");
        my $vc_to_test = "$PROJ_INFO{'RESULT_DIR'}/$test_name.vc";

        if(-e $vc_to_test)
        {
            my $result = `z3 -smt2 $vc_to_test`;
            if($result =~ /^unsat/)
            {
                &info_print(0, ("Verified".((scalar keys %TEST_QUEUE > 1) ? " for $test_name" : "")));
            }
            elsif($result =~ /^sat/)
            {
                &info_print(0, ("Not verified".((scalar keys %TEST_QUEUE > 1) ? " for $test_name" : "")));
            }
            elsif($result =~ 'No such file or directory')
            {
                die "[error] please provide full absolute path to z3 in \$PATH";
            }
            else
            {
                &info_print(1, ("Not verified".((scalar keys %TEST_QUEUE > 1) ? " for $test_name" : "")));
            }
        }
        else
        {
            &info_print(1, "VC doesn't exist for $test_name");
        }
    }
}

####################################################################################################

# Misc.

####################################################################################################


sub get_script_path()
{
    (my $final_path = $+{path}) =~ s/\/$// if(abs_path($0) =~ /(?<path>\/.+\/)/);
    return $final_path;
}

sub mkdir_die()
{
    (my $dirpath) = @_;

    mkdir $dirpath if !-e $dirpath;
    die "[error] unable to create dir at $dirpath" if !-e $dirpath;
}

sub info_print()
{
    my ($level, $string) = @_;
    if($INFO_LEVEL >= $level)
    {
        my $prefix = '';
        if($level == 5)
        {
            $prefix = '[INFO] ';
        }
        elsif($level == 4)
        {
            $prefix = '[INFO] ';
        }
        elsif($level == 3)
        {
            $prefix = '[WARNING] ';
        }
        elsif($level == 2)
        {
            $prefix = '[CRITICAL] ';
        }
        elsif($level == 1)
        {
            $prefix = '[BUG] ';
        }
        elsif($level == 0)
        {
            $prefix = '';
        }

        say "${prefix}${string}";
    }
}
