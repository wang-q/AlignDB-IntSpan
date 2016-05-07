[![Build Status](https://travis-ci.org/wang-q/AlignDB-IntSpan.svg?branch=master)](https://travis-ci.org/wang-q/AlignDB-IntSpan)
[![Cpan version](https://img.shields.io/cpan/v/AlignDB-IntSpan.svg)](https://metacpan.org/release/AlignDB-IntSpan)

# NAME

AlignDB::IntSpan - Handling of sets containing integer spans.

# SYNOPSIS

    use AlignDB::IntSpan;

    my $set = AlignDB::IntSpan->new;
    $set->add(1, 2, 3, 5, 7, 9);
    $set->add_range(100, 1_000_000);
    print $set->as_string, "\n";    # 1-3,5,7,9,100-1000000

## Operator overloads

    if ($set) { ... }   # true if $set is not empty

    print "$set\n";     # stringizes to the run list

# DESCRIPTION

The `AlignDB::IntSpan` module represents sets of integers as a number of
inclusive ranges, for example '1-10,19-23,45-48'. Because many of its
operations involve linear searches of the list of ranges its overall
performance tends to be proportional to the number of distinct ranges. This is
fine for small sets but suffers compared to other possible set representations
(bit vectors, hash keys) when the number of ranges grows large.

This module also represents sets as ranges of values but stores those ranges
in order and uses a binary search for many internal operations so that overall
performance tends towards O log N where N is the number of ranges.

The internal representation used by this module is extremely simple: a set is
represented as a list of integers. Integers in even numbered positions (0, 2,
4 etc) represent the start of a run of numbers while those in odd numbered
positions represent the ends of runs. As an example the set (1, 3-7, 9, 11,
12) would be represented internally as (1, 2, 3, 8, 11, 13).

Sets may be infinite - assuming you're prepared to accept that infinity is
actually no more than a fairly large integer. Specifically the constants
`$NEG_INF` and `$POS_INF` are defined to be -(2^31-1) and (2^31-2)
respectively. To create an infinite set invert an empty one:

    my $inf = AlignDB::IntSpan->new->complement;

Sets need only be bounded in one direction - for example this is the set of
all positive integers (assuming you accept the slightly feeble definition of
infinity we're using):

    my $pos_int = AlignDB::IntSpan->new;
    $pos_int->add_range(1, $pos_int->POS_INF);

Many codes come from [Set::IntSpan](https://metacpan.org/pod/Set::IntSpan), [Set::IntSpan::Fast](https://metacpan.org/pod/Set::IntSpan::Fast) and
[Set::IntSpan::Island](https://metacpan.org/pod/Set::IntSpan::Island).

# METHODS

## **CONSTANTS**

## POS\_INF

Normally used in construction of infinite sets

## NEG\_INF

Normally used in construction of infinite sets

## EMPTY\_STRING

## **INTERFACE: Set creation**

## new

    my $set = AlignDB::Intspan->new; # empty set
    my $set = AlignDB::Intspan->new($set_spec); # the content of $set_spec
    my $set = AlignDB::Intspan->new(@set_specs); # the union of @set_specs

Creates and returns an AlignDB::IntSpan object.

## valid

    my $ok = AlignDB::IntSpan->valid($run_list);

Returns true if $run\_list is a valid run list.

## clear

    $set->clear;

Clear all contents of $set

## **INTERFACE: Set contents**

## edges\_ref

Return the internal used ArrayRef representing the set.

I don't think you should use this method.

## edges

Return the internal used Array representing the set.

I don't think you should use this method.

## edge\_size

Return the number of edges

## span\_size

Return the number of spans

## as\_string

Return a string representation of the set.

## as\_array

Return an array containing all the members of the set in ascending order.

## **INTERFACE: Span contents**

## ranges

Returns the runs in $set, as a list of ($lower, $upper)

## spans

Returns the runs in $set, as a list of \[$lower, $upper\]

## sets

Returns the runs in $set, as a list of AlignDB::IntSpan objects. The sets in
the list are in order.

## runlists

Returns the runs in $set, as a list of "$lower-$upper"

## **INTERFACE: Set cardinality**

## cardinality

Returns the number of elements in $set.

## is\_empty

Return true if the set is empty.

## is\_not\_empty

Return true if the set is not empty.

## is\_neg\_inf

Return true if the set is negtive infinite.

## is\_pos\_inf

Return true if the set is positive infinite.

## is\_infinite

Return true if the set is infinite.

## is\_finite

Return true if the set is finite.

## is\_universal

Return true if the set contains all integers.

## **INTERFACE: Membership test**

## contains\_all

Return true if the set contains all of the specified numbers.

## contains\_any

Return true if the set contains any of the specified numbers.

## **INTERFACE: Member operations**

## add\_pair

    $set->add_pair($lower, $upper);

Add a pair of inclusive integers to the set.

A pair of arguments constitute a range

## add\_range

    $set->add_range($lower, $upper);

Add the inclusive range of integers to the set.

Multiple ranges may be specified. Each pair of arguments constitute a range

## add\_runlist

    $set->add_runlist($runlist);

Add the specified runlist to the set.

## add

    $set->add($number1, $number2, $number3 ...)
    $set->add($runlist);

Add the specified integers or a runlist to the set.

## invert

    $set = $set->invert;

Complement the set.

Because our notion of infinity is actually disappointingly finite inverting a
finite set results in another finite set. For example inverting the empty set
makes it contain all the integers between $NEG\_INF and $POS\_INF inclusive.

As noted above $NEG\_INF and $POS\_INF are actually just big integers.

## remove\_range

$set->remove\_range($lower, $upper);

Remove the inclusive range of integers to the set.

Multiple ranges may be specified. Each pair of arguments constitute a range.

## remove

    $set->remove($number1, $number2, $number3 ...);
    $set->remove($runlist);

Remove the specified integers or a runlist to the set.

## merge

    $set->merge($another_set);
    $set->merge($set_spec);

Merge the members of the supplied sets or set\_specs into this set.
Any number of sets may be supplied as arguments.

## subtract

    $set->subtract($another_set);
    $set->subtract($set_spec);

Subtract the members of the supplied sets or set\_specs out of this set.
Any number of sets may be supplied as arguments.

## **INTERFACE: Set operations**

## copy

    my $new_set = $set->copy;

Return an identical copy of the set.

## union

Be called either as a method

    my $new_set = $set->union( $other_set );

or as a function:

    my $new_set = AlignDB::IntSpan::union( $set1, $set2, $set3 );

Return a new set that is the union of this set and all of the supplied sets.

## complement

    my $new_set = $set->complement;

Returns a new set that is the complement of this set.

## diff

    my $new_set = $set->diff( $other_set );

Return a set containing all the elements that are in this set but not the
supplied set.

## intersect

Be called either as a method

    my $new_set = $set->intersect( $other_set );

or as a function:

    my $new_set = AlignDB::IntSpan::intersect( $set1, $set2, $set3 );

Return a new set that is the intersection of this set and all the supplied
sets.

## xor

Be called either as a method

    my $new_set = $set->xor( $other_set );

or as a function:

    my $new_set = AlignDB::IntSpan::xor( $set1, $set2, $set3 );

Return a new set that contains all of the members that are in this set or the
supplied set but not both.

Can actually handle more than two setsin which case it returns a set that
contains all the members that are in some of the sets but not all of the sets.

## **INTERFACE: Set comparison**

## equal

Returns true if $set and $set\_spec contain the same elements.

## subset

Returns true if $set is a subset of $set\_spec.

## superset

Returns true if $set is a superset of $set\_spec.

## smaller\_than

Returns true if $set is smaller than $set\_spec.

## larger\_than

Returns true if $set is larger than $set\_spec.

## **INTERFACE: Indexing**

## at

Returns the indexth element of set, index start from "1".
Negtive indices count backwards from the end of the set.

## index

Returns the index fo a element in the set, index start from "1"

## slice

Give two indexes, return a subset.
These indexes must be positive.

## **INTERFACE: Extrema**

## min

Returns the smallest element of $set, or undef if there is none.

## max

Returns the largest element of $set, or undef if there is none.

## **INTERFACE: Utils**

## grep\_set

Evaluates the $code\_ref for each integer in $set (locally setting $\_ to each
integer) and returns an AlignDB::IntSpan object containing those integers for
which the $code\_ref returns TRUE.

## map\_set

Evaluates the $code\_ref for each integer in $set (locally setting $\_ to each
integer) and returns an AlignDB::IntSpan object containing all the integers
returned as results of all those evaluations.

Evaluates the $code\_ref in list context, so each element of $set may produce
zero, one, or more elements in the returned set. The elements may be returned
in any order, and need not be disjoint.

## substr\_span

    my $substring = $set->substr_span($string);

## **INTERFACE: Spans operations**

## banish\_span

## cover

Returns a set consisting of a single span from $set->min to $set->max.

## holes

Returns a set containing all the holes in $set, that is, all the integers that
are in-between spans of $set.

## inset

inset returns a set constructed by removing $n integers from each end of each
span of $set. If $n is negative, then -$n integers are added to each end of
each span.

In the first case, spans may vanish from the set; in the second case, holes
may vanish.

## trim

trim is provided as a synonym for inset.

## pad

pad $set $n is the same as $set->inset( -$n )

## excise

    my $new_set = $set->excise( $minlength )

Removes all spans within $set smaller than $minlength

## fill

    my $new_set = $set->fill( $maxlength )

Fills in all holes in $set smaller than $maxlength

## **INTERFACE: Inter-set operations**

## overlap

    my $overlap_amount = $set->overlap( $another_set );

Returns the size of intersection of two sets. Equivalent to

    $set->intersect( $another_set )->size;

## distance

    my $distance = $set->distance( $another_set );

Returns the distance between sets, measured as follows.

If the sets overlap, then the distance is negative and given by

    $d = - $set->overlap( $another_set )

If the sets do not overlap, $d is positive and given by the distance on the
integer line between the two closest islands of the sets.

## **INTERFACE: Islands**

## find\_islands

    my $island = $set->find_islands( $integer );
    my $new_set = $set->find_islands( $another_set );

Returns a set containing the island in $set containing $integer.
If $integer is not in $set, an empty set is returned.
Returns a set containing all islands in $set intersecting $another\_set.
If $set and $another\_set have an empty intersection, an empty set is returned.

## nearest\_island

    my $island = $set->nearest_island( $integer );
    my $island = $set->nearest_island( $another_set );

Returns the nearest island(s) in $set that contains, but does not overlap
with, $integer. If $integer lies exactly between two islands, then the
returned set contains these two islands.

Returns the nearest island(s) in $set that intersects, but does not overlap
with, $another\_set. If $another\_set lies exactly between two islands, then the
returned set contains these two islands.

## at\_island

    my $island = $set->at_island( $island_index );

Returns the island indexed by $island\_index. Islands are 1-indexed. For a set
with N islands, the first island (ordered left-to-right) has index 1 and the
last island has index N. If $island\_index is negative, counting is done back
from the last island (c.f. negative indexes of Perl arrays).

## **INTERFACE: Aliases**

    runlist, run_list           => as_string

    elements                    => as_array

    size, count                 => cardinality

    empty                       => is_empty

    contains, contain, member   => contains_all

    duplicate                   => copy

    intersection                => intersect

    equals                      => equal

# AUTHOR

Qiang Wang &lt;wang-q@outlook.com>

# COPYRIGHT AND LICENSE

This software is copyright (c) 2008 by Qiang Wang.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.
