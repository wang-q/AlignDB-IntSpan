#!/usr/bin/perl
use warnings;
use strict;

use Time::HiRes qw ( time );

use AlignDB::IntSpan;

sub test_add_range {
    my $step = shift;

    my @vec1 = (
        1,   30,  32,  149, 153, 155, 159, 247, 250, 250, 253, 464,
        516, 518, 520, 523, 582, 585, 595, 600, 622, 1679,
    );
    my @vec2 = ( 100, 1000000 );

    for my $i ( 1 .. 100000 ) {
        my $set = AlignDB::IntSpan->new;
        $set->add_range(@vec1) if $step >= 2;
        $set->add_range(@vec2) if $step >= 3;
        $set->as_string        if $step >= 4;

        if ( $step >= 5 ) {
            for my $j ( 1 .. 200 ) {
                $set->add($j);
            }
        }

        if ( $step >= 6 ) {
            for my $j ( 1 .. 100 ) {
                $set->add_range( $j * 5, $j * 10 );
            }
        }
    }
}

my ( $start, $end );

for my $i ( 2 .. 6 ) {
    printf "step %d\n", $i;
    $start = time();
    test_add_range($i);
    $end = time();
    printf( "start %f end %f duration %f\n", $start, $end, $end - $start );
}
