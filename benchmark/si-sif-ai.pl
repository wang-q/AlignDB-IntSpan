#!/usr/bin/perl
use warnings;
use strict;

use Benchmark qw(:all);
use YAML qw(Dump Load DumpFile LoadFile);

use Set::IntSpan;
use Set::IntSpan::Fast;
use AlignDB::IntSpan;

#----------------------------------------------------------#
# Benchmark 1, including object startup
#----------------------------------------------------------#
{
    my @test_array = (
        1 .. 30,
        32 .. 149,
        153 .. 155,
        159 .. 247,
        250,
        253 .. 464,
        516 .. 518,
        520 .. 523,
        582 .. 585,
        595 .. 600,
        622 .. 1679,
    );

    my $test1 = sub {
        my $step = shift;
        my $set = AlignDB::IntSpan->new if $step >= 1;
        $set->add(@test_array) if ( $step >= 2 );
        $set->add_range( 100, 1_000_000 ) if $step >= 3;
        $set->as_string if $step >= 4;
    };

    my $test2 = sub {
        my $step = shift;
        my $set = Set::IntSpan::Fast->new if $step >= 1;
        $set->add(@test_array) if ( $step >= 2 );
        $set->add_range( 100, 1_000_000 ) if $step >= 3;
        $set->as_string if $step >= 4;
    };

    my $test3 = sub {
        my $step = shift;
        my $set = Set::IntSpan->new if $step >= 1;
        if ( $step >= 2 ) {
            $set->insert($_) for @test_array;
        }
        $set = $set->union( 100 . '-' . 1_000_000 ) if $step >= 3;
        $set->run_list if $step >= 4;
    };

    for my $step ( 1 .. 4 ) {
        print '-' x 60, "\n";
        print "Benchmark 1, including object startup.\nStep $step: \n";
        cmpthese(
            -10,
            {   'AI'  => sub { $test1->($step) },
                'SIF' => sub { $test2->($step) },
                'SI'  => sub { $test3->($step) },
            }
        );
    }

}

#----------------------------------------------------------#
# Benchmark 2, excluding object startup
#----------------------------------------------------------#
{
    my @test_array = (
        1 .. 30,
        32 .. 149,
        153 .. 155,
        159 .. 247,
        250,
        253 .. 464,
        516 .. 518,
        520 .. 523,
        582 .. 585,
        595 .. 600,
        622 .. 1679,
    );

    my $set1 = AlignDB::IntSpan->new;
    my $set2 = Set::IntSpan::Fast->new;
    my $set3 = Set::IntSpan->new;

    my $test1 = sub {
        my $step = shift;
        my $set  = shift;
        $set->add(@test_array) if ( $step >= 2 );
        $set->add_range( 100, 1_000_000 ) if $step >= 3;
        $set->as_string if $step >= 4;
    };

    my $test2 = sub {
        my $step = shift;
        my $set  = shift;
        $set->add(@test_array) if ( $step >= 2 );
        $set->add_range( 100, 1_000_000 ) if $step >= 3;
        $set->as_string if $step >= 4;
    };

    my $test3 = sub {
        my $step = shift;
        my $set  = shift;
        if ( $step >= 2 ) {
            $set->insert($_) for (@test_array);
        }
        $set = $set->union( 100 . '-' . 1_000_000 ) if $step >= 3;
        $set->run_list if $step >= 4;
    };

    for my $step ( 2 .. 4 ) {
        print '-' x 60, "\n";
        print "Benchmark 2, excluding object startup.\nStep $step: \n";
        cmpthese(
            -10,
            {   'AI'  => sub { $test1->( $step, $set1 ) },
                'SIF' => sub { $test2->( $step, $set2 ) },
                'SI'  => sub { $test3->( $step, $set3 ) },
            }
        );
    }
}

#----------------------------------------------------------#
# Benchmark 3, incremental insertion
#----------------------------------------------------------#
{
    my @test_array = ( 1 .. 500, 800 .. 1000 );

    my $test1 = sub {
        my $set = shift;
        $set->add(@test_array);
    };

    my $test2 = sub {
        my $set = shift;
        $set->add(@test_array);
    };

    my $test3 = sub {
        my $set = shift;
        $set->insert($_) for (@test_array);
    };

    my $set1 = AlignDB::IntSpan->new;
    my $set2 = Set::IntSpan::Fast->new;
    my $set3 = Set::IntSpan->new;

    print '-' x 60, "\n";
    print "Benchmark 3, incremental insertion.\n";
    cmpthese(
        -10,
        {   'AI'  => sub { $test1->($set1) },
            'SIF' => sub { $test2->($set2) },
            'SI'  => sub { $test3->($set3) },
        }
    );
}

#----------------------------------------------------------#
# Benchmark 4, incremental union
#----------------------------------------------------------#
{
    my $test1 = sub {
        my $set = shift;
        $set->add_range( 5 * $_, 10 * $_ ) for ( 1 .. 100 );
    };

    my $test2 = sub {
        my $set = shift;
        $set->add_range( 5 * $_, 10 * $_ ) for ( 1 .. 100 );
    };

    my $test3 = sub {
        my $set = shift;
        $set->union( 10 * ( $_ - 1 ) . '-' . 10 * $_ ) for ( 1 .. 100 );
    };

    my $set1 = AlignDB::IntSpan->new;
    my $set2 = Set::IntSpan::Fast->new;
    my $set3 = Set::IntSpan->new;

    print '-' x 60, "\n";
    print "Benchmark 4, incremental union.\n";
    cmpthese(
        -10,
        {   'AI'  => sub { $test1->($set1) },
            'SIF' => sub { $test2->($set2) },
            'SI'  => sub { $test3->($set3) },
        }
    );
}
