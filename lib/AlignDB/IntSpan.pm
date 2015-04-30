package AlignDB::IntSpan;

# ABSTRACT: Handling of sets containing integer spans.

use strict;
use warnings;
use Carp;

use Readonly;
use Scalar::Util qw(blessed);
use Scalar::Util::Numeric qw(isint);

use overload (
    q{0+} => sub { confess "Can't numerify an AlignDB::IntSpan\n" },
    q{bool} => q{is_not_empty},
    q{""}   => q{runlist},

    # use Perl standard behaviours for other operations
    fallback => 1,
);

=method B<CONSTANTS>

=cut

Readonly my $POS_INF => 2_147_483_647 - 1;             # INT_MAX - 1
Readonly my $NEG_INF => ( -2_147_483_647 - 1 ) + 1;    # INT_MIN + 1

=method POS_INF

Normally used in construction of infinite sets

=cut

sub POS_INF {
    return $POS_INF - 1;
}

=method NEG_INF

Normally used in construction of infinite sets

=cut

sub NEG_INF {
    return $NEG_INF;
}

=method EMPTY_STRING

=cut

sub EMPTY_STRING {
    return '-';
}

=method B<INTERFACE: Set creation>

=method new

    my $set = AlignDB::Intspan->new; # empty set
    my $set = AlignDB::Intspan->new($set_spec); # the content of $set_spec
    my $set = AlignDB::Intspan->new(@set_specs); # the union of @set_specs

Creates and returns an AlignDB::IntSpan object.

=cut

sub new {
    my $class = shift;
    my $self  = {};
    $self->{edges} = [];
    bless $self, $class;
    $self->add(@_) if @_ > 0;
    return $self;
}

=method valid

    my $ok = AlignDB::IntSpan->valid($run_list);

Returns true if $run_list is a valid run list.

=cut

sub valid {
    my $this    = shift;
    my $runlist = shift;

    my $class = ref($this) || $this;
    my $set = new $class;

    eval { $set->_runlist_to_ranges($runlist) };
    return $@ ? 0 : 1;
}

=method clear

    $set->clear;

Clear all contents of $set

=cut

sub clear {
    my $self = shift;
    $self->{edges} = [];
    return $self;
}

=method B<INTERFACE: Set contents>

=method edges_ref

Return the internal used ArrayRef representing the set.

I don't think you should use this method.

=cut

sub edges_ref {
    my $self = shift;
    return $self->{edges};
}

=method edges

Return the internal used Array representing the set.

I don't think you should use this method.

=cut

sub edges {
    my $self = shift;
    return @{ $self->edges_ref };
}

=method edge_size

Return the number of edges

=cut

sub edge_size {
    my $self = shift;
    return scalar $self->edges;
}

=method span_size

Return the number of spans

=cut

sub span_size {
    my $self = shift;
    return $self->edge_size / 2;
}

=method as_string

Return a string representation of the set.

=cut

sub as_string {
    my $self = shift;

    if ( $self->is_empty ) {
        return $self->EMPTY_STRING;
    }

    my @runs;
    my @ranges = $self->ranges;
    while (@ranges) {
        my $lower = shift @ranges;
        my $upper = shift @ranges;
        push @runs, $lower == $upper ? $lower : "$lower-$upper";
    }

    return join( ',', @runs );
}

=method as_array

Return an array containing all the members of the set in ascending order.

=cut

sub as_array {
    my $self = shift;

    my @elements;
    my @ranges = $self->ranges;
    while (@ranges) {
        my $lower = shift @ranges;
        my $upper = shift @ranges;
        push @elements, ( $lower .. $upper );
    }

    return @elements;
}

=method B<INTERFACE: Span contents>

=method ranges

Returns the runs in $set, as a list of ($lower, $upper)

=cut

sub ranges {
    my $self = shift;

    my @ranges;
    my @edges = $self->edges;
    while (@edges) {
        my $lower = shift @edges;
        my $upper = shift(@edges) - 1;
        push @ranges, ( $lower, $upper );
    }

    return @ranges;
}

=method spans

Returns the runs in $set, as a list of [$lower, $upper]

=cut

sub spans {
    my $self = shift;

    my @spans;
    my @ranges = $self->ranges;
    while (@ranges) {
        my $lower = shift @ranges;
        my $upper = shift @ranges;
        push @spans, [ $lower, $upper ];
    }

    return @spans;
}

=method sets

Returns the runs in $set, as a list of AlignDB::IntSpan objects. The sets in
the list are in order.

=cut

sub sets {
    my $self = shift;

    my @sets;
    my @ranges = $self->ranges;
    while (@ranges) {
        my $lower = shift @ranges;
        my $upper = shift @ranges;
        push @sets, blessed($self)->new("$lower-$upper");
    }

    return @sets;
}

=method runlists

Returns the runs in $set, as a list of "$lower-$upper"

=cut

sub runlists {
    my $self = shift;

    if ( $self->is_empty ) {
        return $self->EMPTY_STRING;
    }

    my @runlists;
    my @ranges = $self->ranges;
    while (@ranges) {
        my $lower  = shift @ranges;
        my $upper  = shift @ranges;
        my $string = $lower == $upper ? $lower : $lower . '-' . $upper;
        push @runlists, $string;
    }

    return @runlists;
}

=method B<INTERFACE: Set cardinality>

=method cardinality

Returns the number of elements in $set.

=cut

sub cardinality {
    my $self = shift;

    my $cardinality = 0;
    my @ranges      = $self->ranges;
    while (@ranges) {
        my $lower = shift @ranges;
        my $upper = shift @ranges;
        $cardinality += $upper - $lower + 1;
    }

    return $cardinality;
}

=method is_empty

Return true if the set is empty.

=cut

sub is_empty {
    my $self = shift;
    my $result = $self->edge_size == 0 ? 1 : 0;
    return $result;
}

=method is_not_empty

Return true if the set is not empty.

=cut

sub is_not_empty {
    my $self = shift;
    return !$self->is_empty;
}

=method is_neg_inf

Return true if the set is negtive infinite.

=cut

sub is_neg_inf {
    my $self = shift;
    return $self->edges_ref->[0] == $NEG_INF;
}

=method is_pos_inf

Return true if the set is positive infinite.

=cut

sub is_pos_inf {
    my $self = shift;
    return $self->edges_ref->[-1] == $POS_INF;
}

=method is_infinite

Return true if the set is infinite.

=cut

sub is_infinite {
    my $self = shift;
    return $self->is_neg_inf || $self->is_pos_inf;
}

=method is_finite

Return true if the set is finite.

=cut

sub is_finite {
    my $self = shift;
    return !$self->is_infinite;
}

=method is_universal

Return true if the set contains all integers.

=cut

sub is_universal {
    my $self = shift;
    return $self->edge_size == 2 && $self->is_neg_inf && $self->is_pos_inf;
}

=method B<INTERFACE: Membership test>

=method contains_all

Return true if the set contains all of the specified numbers.

=cut

sub contains_all {
    my $self = shift;

    for my $i (@_) {
        my $pos = $self->_find_pos( $i + 1, 0 );
        return 0 unless $pos & 1;
    }

    return 1;
}

=method contains_any

Return true if the set contains any of the specified numbers.

=cut

sub contains_any {
    my $self = shift;

    for my $i (@_) {
        my $pos = $self->_find_pos( $i + 1, 0 );
        return 1 if $pos & 1;
    }

    return 0;
}

=method B<INTERFACE: Member operations>

=cut

=method add_pair

    $set->add_pair($lower, $upper);
 
Add a pair of inclusive integers to the set.

A pair of arguments constitute a range

=cut

sub add_pair {
    my $self   = shift;
    my @ranges = @_;

    if ( scalar(@ranges) != 2 ) {
        confess "Number of ranges must be two: @ranges\n";
    }

    my $edges_ref = $self->edges_ref;

    my $from = shift @ranges;
    my $to   = shift(@ranges) + 1;
    if ( $from > $to ) {
        confess "Bad order: $from-$to\n";
    }
    my $from_pos = $self->_find_pos( $from,   0 );
    my $to_pos   = $self->_find_pos( $to + 1, $from_pos );

    if ( $from_pos & 1 ) {
        $from = $edges_ref->[ --$from_pos ];
    }
    if ( $to_pos & 1 ) {
        $to = $edges_ref->[ $to_pos++ ];
    }

    splice @{$edges_ref}, $from_pos, $to_pos - $from_pos, ( $from, $to );

    return $self;
}

=method add_range

    $set->add_range($lower, $upper);

Add the inclusive range of integers to the set.

Multiple ranges may be specified. Each pair of arguments constitute a range

=cut

sub add_range {
    my $self   = shift;
    my @ranges = @_;

    if ( scalar(@ranges) % 2 == 1 ) {
        confess "Number of ranges must be even: @ranges\n";
    }

    while (@ranges) {
        my $from = shift @ranges;
        my $to   = shift @ranges;
        $self->add_pair( $from, $to );
    }

    return $self;
}

=method add_runlist

    $set->add_runlist($runlist);

Add the specified runlist to the set.

=cut

sub add_runlist {
    my $self  = shift;
    my $first = shift;

    $self->add_range( $self->_runlist_to_ranges($first) );

    return $self;
}

=method add

    $set->add($number1, $number2, $number3 ...)
    $set->add($runlist);

Add the specified integers or a runlist to the set.

=cut

sub add {
    my $self  = shift;
    my $first = shift;

    if ( ref $first eq ref $self ) {
        $self->add_range( $first->ranges );
    }
    elsif ( isint($first) ) {
        if ( scalar @_ > 0 ) {
            $self->add_range( $self->_list_to_ranges( $first, @_ ) );
        }
        else {
            $self->add_pair( $first, $first );
        }
    }
    else {
        $self->add_range( $self->_runlist_to_ranges($first) );
    }

    return $self;
}

=method invert

    $set = $set->invert;

Complement the set.

Because our notion of infinity is actually disappointingly finite inverting a
finite set results in another finite set. For example inverting the empty set
makes it contain all the integers between $NEG_INF and $POS_INF inclusive.

As noted above $NEG_INF and $POS_INF are actually just big integers.

=cut

sub invert {
    my $self = shift;

    # $edges_ref is an ArrayRef, which points to the same array as the
    #   'edges' attribute. So manipulate $edges_ref affects the attribute
    my $edges_ref = $self->edges_ref;

    if ( $self->is_empty ) {
        $self->{edges} = [ $NEG_INF, $POS_INF ];    # Universal set
    }
    else {

        # Either add or remove infinity from each end. The net
        # effect is always an even number of additions and deletions
        if ( $edges_ref->[0] == $NEG_INF ) {
            shift @{$edges_ref};
        }
        else {
            unshift @{$edges_ref}, $NEG_INF;
        }

        if ( $edges_ref->[-1] == $POS_INF ) {
            pop @{$edges_ref};
        }
        else {
            push @{$edges_ref}, $POS_INF;
        }
    }

    return $self;
}

=method remove_range

$set->remove_range($lower, $upper);

Remove the inclusive range of integers to the set.

Multiple ranges may be specified. Each pair of arguments constitute a range.

=cut

sub remove_range {
    my $self = shift;

    $self->invert;
    $self->add_range(@_);
    $self->invert;

    return $self;
}

=method remove

    $set->remove($number1, $number2, $number3 ...);
    $set->remove($runlist);

Remove the specified integers or a runlist to the set. 

=cut

sub remove {
    my $self  = shift;
    my $first = shift;
    
    if ( ref $first eq ref $self ) {
        $self->remove_range( $first->ranges );
    }
    elsif ( isint($first) ) {
        if ( scalar @_ > 0 ) {
            $self->remove_range( $self->_list_to_ranges( $first, @_ ) );
        }
        else {
            $self->remove_range( $first, $first );
        }
    }
    else {
        $self->remove_range( $self->_runlist_to_ranges($first) );
    }

    return $self;
}

=method merge

    $set->merge($another_set);
    $set->merge($set_spec);

Merge the members of the supplied sets or set_specs into this set.
Any number of sets may be supplied as arguments.

=cut

sub merge {
    my $self = shift;

    foreach my $supplied (@_) {
        my @ranges = $self->_real_set($supplied)->ranges;
        $self->add_range(@ranges);
    }

    return $self;
}

=method subtract

    $set->subtract($another_set);
    $set->subtract($set_spec);

Subtract the members of the supplied sets or set_specs out of this set.
Any number of sets may be supplied as arguments.

=cut

sub subtract {
    my $self = shift;

    foreach my $supplied (@_) {
        my @ranges = $self->_real_set($supplied)->ranges;
        $self->remove_range(@ranges);
    }

    return $self;
}

=method B<INTERFACE: Set operations>

=method copy

    my $new_set = $set->copy;

Return an identical copy of the set.

=cut

sub copy {
    my $self = shift;

    my $copy = blessed($self)->new;
    $copy->{edges} = [ $self->edges ];

    return $copy;
}

=method union

Be called either as a method

    my $new_set = $set->union( $other_set );

or as a function:

    my $new_set = AlignDB::IntSpan::union( $set1, $set2, $set3 );

Return a new set that is the union of this set and all of the supplied sets.

=cut

sub union {
    my $self = shift;

    my $new = $self->copy;
    $new->merge(@_);

    return $new;
}

=method complement

    my $new_set = $set->complement;

Returns a new set that is the complement of this set.

=cut

sub complement {
    my $self = shift;

    my $new = $self->copy;
    $new->invert;

    return $new;
}

=method diff

    my $new_set = $set->diff( $other_set );

Return a set containing all the elements that are in this set but not the
supplied set.

=cut

sub diff {
    my $self = shift;

    my $new = $self->copy;
    $new->subtract(@_);

    return $new;
}

=method intersect

Be called either as a method

    my $new_set = $set->intersect( $other_set );

or as a function:


    my $new_set = AlignDB::IntSpan::intersect( $set1, $set2, $set3 );

Return a new set that is the intersection of this set and all the supplied
sets.

=cut

sub intersect {
    my $self = shift;

    my $new = $self->complement;
    for my $supplied (@_) {
        my $temp_set = $self->_real_set($supplied)->complement;
        $new->merge($temp_set);
    }
    $new->invert;

    return $new;
}

=method xor

Be called either as a method

    my $new_set = $set->xor( $other_set );

or as a function:

    my $new_set = AlignDB::IntSpan::xor( $set1, $set2, $set3 );

Return a new set that contains all of the members that are in this set or the
supplied set but not both. 

Can actually handle more than two setsin which case it returns a set that
contains all the members that are in some of the sets but not all of the sets.

=cut

sub xor {
    return intersect( union(@_), intersect(@_)->complement );
}

=method B<INTERFACE: Set comparison>

=method equal

Returns true if $set and $set_spec contain the same elements.

=cut

sub equal {
    my $self = shift;

    for (@_) {
        my $supplied = $self->_real_set($_);

        if ( $self->edge_size != $supplied->edge_size ) {
            return 0;
        }

        my @edges_a = $self->edges;
        my @edges_b = $supplied->edges;

        for ( my $i = 0; $i < $self->edge_size; $i++ ) {
            if ( $edges_a[$i] != $edges_b[$i] ) {
                return 0;
            }
        }
    }

    return 1;
}

=method subset

Returns true if $set is a subset of $set_spec.

=cut

sub subset {
    my $self     = shift;
    my $supplied = $self->_real_set(shift);

    return $self->diff($supplied)->is_empty;
}

=method superset

Returns true if $set is a superset of $set_spec.

=cut

sub superset {
    my $self     = shift;
    my $supplied = $self->_real_set(shift);

    return $supplied->diff($self)->is_empty;
}

=method smaller_than

Returns true if $set is smaller than $set_spec.

=cut

sub smaller_than {
    my $self     = shift;
    my $supplied = shift;

    my $result = $self->subset($supplied) && !$self->equal($supplied);

    return $result ? 1 : 0;
}

=method larger_than

Returns true if $set is larger than $set_spec.

=cut

sub larger_than {
    my $self     = shift;
    my $supplied = shift;

    my $result = $self->superset($supplied) && !$self->equal($supplied);

    return $result ? 1 : 0;
}

=method B<INTERFACE: Indexing>

=method at

Returns the indexth element of set, index start from "1".
Negtive indices count backwards from the end of the set.

=cut

sub at {
    my $self  = shift;
    my $index = shift;
    if ( $index == 0 || abs($index) > $self->cardinality ) {
        return;
    }
    my $member
        = $index < 0 ? $self->_at_neg( -$index ) : $self->_at_pos($index);
    return $member;
}

sub _at_pos {
    my $self  = shift;
    my $index = shift;

    my $member;
    my $element_before = 0;

    my @ranges = $self->ranges;
    while (@ranges) {
        my $lower     = shift @ranges;
        my $upper     = shift @ranges;
        my $span_size = $upper - $lower + 1;

        if ( $index > $element_before + $span_size ) {
            $element_before += $span_size;
        }
        else {
            $member = $index - $element_before - 1 + $lower;
            last;
        }
    }

    return $member;
}

sub _at_neg {
    my $self  = shift;
    my $index = shift;

    my $member;
    my $element_after = 0;

    my @r_ranges = reverse $self->ranges;
    while (@r_ranges) {
        my $upper     = shift @r_ranges;
        my $lower     = shift @r_ranges;
        my $span_size = $upper - $lower + 1;

        if ( $index > $element_after + $span_size ) {
            $element_after += $span_size;
        }
        else {
            $member = $upper - ( $index - $element_after ) + 1;
            last;
        }
    }

    return $member;
}

=method lookup_back_index

Give an backword index, return a element

=cut

sub lookup_back_index {
    my $self  = shift;
    my $index = shift;
    return $self->at( -$index );
}

=method index

Returns the index fo a element in the set, index start from "1"

=cut

sub index {
    my $self   = shift;
    my $member = shift;

    my $index;
    my $element_before = 0;

    my @ranges = $self->ranges;
    while (@ranges) {
        my $lower     = shift @ranges;
        my $upper     = shift @ranges;
        my $span_size = $upper - $lower + 1;

        if ( $member >= $lower and $member <= $upper ) {
            $index = $member - $lower + 1 + $element_before;
            last;
        }
        else {
            $element_before += $span_size;
        }
    }

    return $index;
}

=method slice

Give two indexes, return a subset.
These indexes must be positive.

=cut

sub slice {
    my $self = shift;
    my $from = shift;
    my $to   = shift;

    if ( $from < 1 ) {
        carp "Start index less than 1\n";
        $from = 1;
    }
    my $slice = $self->_splice( $from, $to - $from + 1 );

    return $slice;
}

sub _splice {
    my $self   = shift;
    my $offset = shift;
    my $length = shift;

    my @edges = $self->edges;
    my $slice = blessed($self)->new;

    while ( @edges > 1 ) {
        my ( $lower, $upper ) = @edges[ 0, 1 ];
        my $span_size = $upper - $lower;

        if ( $offset <= $span_size ) {
            last;
        }
        else {
            splice( @edges, 0, 2 );
            $offset -= $span_size;
        }
    }

    @edges
        or return $slice;    # empty set

    $edges[0] += $offset - 1;

    my @slices = $self->_splice_length( \@edges, $length );
    while (@slices) {
        my $lower = shift @slices;
        my $upper = shift(@slices) - 1;
        $slice->add_pair( $lower, $upper );
    }

    return $slice;
}

sub _splice_length {
    my $self      = shift;
    my $edges_ref = shift;
    my $length    = shift;

    if ( !defined $length ) {
        return @{$edges_ref};    # everything
    }

    if ( $length <= 0 ) {
        return ();               # empty
    }

    my @slices;

    while ( @$edges_ref > 1 ) {
        my ( $lower, $upper ) = @$edges_ref[ 0, 1 ];
        my $span_size = $upper - $lower;

        if ( $length <= $span_size ) {
            last;
        }
        else {
            push @slices, splice( @$edges_ref, 0, 2 );
            $length -= $span_size;
        }
    }

    if (@$edges_ref) {
        my $lower = shift @$edges_ref;
        push @slices, $lower, $lower + $length;
    }

    return @slices;
}

=method B<INTERFACE: Extrema>

=method min

Returns the smallest element of $set, or undef if there is none.

=cut

sub min {
    my $self = shift;

    if ( $self->is_empty ) {
        return;
    }
    else {
        return $self->edges_ref->[0];
    }
}

=method max

Returns the largest element of $set, or undef if there is none.

=cut

sub max {
    my $self = shift;

    if ( $self->is_empty ) {
        return;
    }
    else {
        return $self->edges_ref->[-1] - 1;
    }
}

=method B<INTERFACE: Utils>

=method grep_set

Evaluates the $code_ref for each integer in $set (locally setting $_ to each
integer) and returns an AlignDB::IntSpan object containing those integers for
which the $code_ref returns TRUE.

=cut

sub grep_set {
    my $self     = shift;
    my $code_ref = shift;

    my @sub_elements;
    for ( $self->elements ) {
        if ( $code_ref->() ) {
            push @sub_elements, $_;
        }

    }
    my $sub_set = blessed($self)->new(@sub_elements);

    return $sub_set;
}

=method map_set

Evaluates the $code_ref for each integer in $set (locally setting $_ to each
integer) and returns an AlignDB::IntSpan object containing all the integers
returned as results of all those evaluations.

Evaluates the $code_ref in list context, so each element of $set may produce
zero, one, or more elements in the returned set. The elements may be returned
in any order, and need not be disjoint.

=cut

sub map_set {
    my $self     = shift;
    my $code_ref = shift;

    my @map_elements;
    for ( $self->elements ) {
        foreach my $element ( $code_ref->() ) {
            if ( defined $element ) {
                push @map_elements, $element;
            }
        }

    }
    my $map_set = blessed($self)->new(@map_elements);

    return $map_set;
}

=method substr_span

    my $substring = $set->substr_span($string);

=cut

sub substr_span {
    my $self   = shift;
    my $string = shift;

    my $sub_string = "";
    my @spans      = $self->spans;

    foreach (@spans) {
        my ( $lower, $upper ) = @$_;
        my $length = $upper - $lower + 1;

        $sub_string .= substr( $string, $lower - 1, $length );
    }

    return $sub_string;
}

=method B<INTERFACE: Spans operations>

=method banish_span

=cut

sub banish_span {
    my $self  = shift;
    my $start = shift;
    my $end   = shift;

    my $remove_length = $end - $start + 1;

    my $new = $self->map_set(
        sub {
                  $_ < $start ? $_
                : $_ > $end   ? $_ - $remove_length
                :               ();
        }
    );

    return $new;
}

=method cover

Returns a set consisting of a single span from $set->min to $set->max.

=cut

sub cover {
    my $self = shift;

    my $cover = blessed($self)->new;
    if ( $self->is_not_empty ) {
        $cover->add_pair( $self->min, $self->max );
    }
    return $cover;
}

=method holes

Returns a set containing all the holes in $set, that is, all the integers that
are in-between spans of $set.

=cut

sub holes {
    my $self = shift;

    my $holes     = blessed($self)->new;

    if ( $self->is_empty or $self->is_universal ) {

        # empty set and universal set have no holes
    }
    else {
        my $c_set = $self->complement;
        my @ranges = $c_set->ranges;

        # Remove infinite arms of complement set
        if ( $c_set->is_neg_inf ) {
            
            shift @ranges;
            shift @ranges;
        }
        if ( $c_set->is_pos_inf ) {
            pop @ranges;
            pop @ranges;
        }
        $holes->add_range(@ranges);
    }

    return $holes;
}

=method inset

inset returns a set constructed by removing $n integers from each end of each
span of $set. If $n is negative, then -$n integers are added to each end of
each span.

In the first case, spans may vanish from the set; in the second case, holes
may vanish.

=cut

sub inset {
    my $self = shift;
    my $n    = shift;

    my $inset  = blessed($self)->new;
    my @ranges = $self->ranges;
    while (@ranges) {
        my $lower = shift @ranges;
        my $upper = shift @ranges;
        if ( $lower != $self->NEG_INF ) {
            $lower += $n;
        }
        if ( $upper != $self->POS_INF ) {
            $upper -= $n;
        }
        $inset->add_pair( $lower, $upper )
            if $lower <= $upper;
    }

    return $inset;
}

=method trim

trim is provided as a synonym for inset.

=cut

sub trim {
    my $self = shift;
    my $n    = shift;
    return $self->inset($n);
}

=method pad

pad $set $n is the same as $set->inset( -$n )

=cut

sub pad {
    my $self = shift;
    my $n    = shift;
    return $self->inset( -$n );
}

=method excise

    my $new_set = $set->excise( $minlength )

Removes all spans within $set smaller than $minlength

=cut

sub excise {
    my $self      = shift;
    my $minlength = shift;

    my $set = blessed($self)->new;
    map { $set->merge($_) } grep { $_->size >= $minlength } $self->sets;

    return $set;
}

=method fill

    my $new_set = $set->fill( $maxlength )

Fills in all holes in $set smaller than $maxlength

=cut

sub fill {
    my $self      = shift;
    my $maxlength = shift;

    my $set = $self->copy;
    if ( $maxlength > 0 ) {
        for my $hole ( $set->holes->sets ) {
            if ( $hole->size <= $maxlength ) {
                $set->merge($hole);
            }
        }
    }
    return $set;
}

=method B<INTERFACE: Inter-set operations>

=cut

=method overlap

    my $overlap_amount = $set->overlap( $another_set );

Returns the size of intersection of two sets. Equivalent to

    $set->intersect( $another_set )->size;

=cut

sub overlap {
    my $self     = shift;
    my $supplied = shift;
    return $self->intersect($supplied)->size;
}

=method distance

    my $distance = $set->distance( $another_set );

Returns the distance between sets, measured as follows.

If the sets overlap, then the distance is negative and given by

    $d = - $set->overlap( $another_set )

If the sets do not overlap, $d is positive and given by the distance on the
integer line between the two closest islands of the sets.

=cut

sub distance {
    my $self     = shift;
    my $supplied = shift;

    return unless $self->size and $supplied->size;

    my $overlap = $self->overlap($supplied);
    return -$overlap if $overlap;

    my $min_d;
    for my $span1 ( $self->sets ) {
        for my $span2 ( $supplied->sets ) {
            my $d1 = abs( $span1->min - $span2->max );
            my $d2 = abs( $span1->max - $span2->min );
            my $d  = $d1 < $d2 ? $d1 : $d2;
            if ( !defined $min_d or $d < $min_d ) {
                $min_d = $d;
            }
        }
    }

    return $min_d;
}

=method B<INTERFACE: Islands>

=method find_islands

    my $island = $set->find_islands( $integer );
    my $new_set = $set->find_islands( $another_set );

Returns a set containing the island in $set containing $integer.
If $integer is not in $set, an empty set is returned.
Returns a set containing all islands in $set intersecting $another_set.
If $set and $another_set have an empty intersection, an empty set is returned.

=cut

sub find_islands {
    my $self     = shift;
    my $supplied = shift;

    my $island;
    if ( ref $supplied eq ref $self ) {
        $island = $self->_find_islands_set($supplied);
    }
    elsif ( isint($supplied) ) {
        $island = $self->_find_islands_int($supplied);
    }
    else {
        confess "Don't know how to deal with input to find_island\n";
    }

    return $island;
}

sub _find_islands_int {
    my $self   = shift;
    my $number = shift;

    my $island = blessed($self)->new;

    # if $pos & 1, i.e. $pos is odd number, $val is in the set
    my $pos = $self->_find_pos( $number + 1, 0 );
    if ( $pos & 1 ) {
        my @ranges = $self->ranges;
        $island->add_range( $ranges[ $pos - 1 ], $ranges[$pos] );
    }

    return $island;
}

sub _find_islands_set {
    my $self     = shift;
    my $supplied = shift;

    my $islands = blessed($self)->new;

    if ( $self->overlap($supplied) ) {
        for my $subset ( $self->sets ) {
            $islands->merge($subset) if $subset->overlap($supplied);
        }
    }

    return $islands;
}

=method nearest_island

    my $island = $set->nearest_island( $integer );
    my $island = $set->nearest_island( $another_set );

Returns the nearest island(s) in $set that contains, but does not overlap
with, $integer. If $integer lies exactly between two islands, then the
returned set contains these two islands.

Returns the nearest island(s) in $set that intersects, but does not overlap
with, $another_set. If $another_set lies exactly between two islands, then the
returned set contains these two islands.

=cut

sub nearest_island {
    my $self     = shift;
    my $supplied = shift;

    if ( ref $supplied eq ref $self ) {    # just OK
    }
    elsif ( isint($supplied) ) {
        $supplied = blessed($self)->new($supplied);
    }
    else {
        confess "Don't know how to deal with input to nearest_island\n";
    }

    my $island = blessed($self)->new;
    my $min_d;
    for my $s ( $self->sets ) {
        for my $ss ( $supplied->sets ) {
            next if $s->overlap($ss);
            my $d = $s->distance($ss);
            if ( !defined $min_d or $d <= $min_d ) {
                if ( defined $min_d and $d == $min_d ) {
                    $island->merge($s);
                }
                else {
                    $min_d  = $d;
                    $island = $s->copy;
                }
            }
        }
    }

    return $island;
}

=method at_island

    my $island = $set->at_island( $island_index );

Returns the island indexed by $island_index. Islands are 1-indexed. For a set
with N islands, the first island (ordered left-to-right) has index 1 and the
last island has index N. If $island_index is negative, counting is done back
from the last island (c.f. negative indexes of Perl arrays).

=cut

sub at_island {
    my $self  = shift;
    my $index = shift;

    return if $index == 0 or abs($index) > $self->span_size;

    my @islands = $self->sets;

    return $index < 0 ? $islands[$index] : $islands[ $index - 1 ];
}

#----------------------------------------------------------#
# Internal methods
#----------------------------------------------------------#
# Converts a list of integers into pairs of ranges
sub _list_to_ranges {
    my $self = shift;

    my @list = sort { $a <=> $b } @_;
    my @ranges;
    my $count = scalar @list;
    my $pos   = 0;
    while ( $pos < $count ) {
        my $end = $pos + 1;
        $end++ while $end < $count && $list[$end] <= $list[ $end - 1 ] + 1;
        push @ranges, ( $list[$pos], $list[ $end - 1 ] );
        $pos = $end;
    }

    return @ranges;
}

# Converts a runlist into pairs of ranges
sub _runlist_to_ranges {
    my $self = shift;

    my $runlist = shift;
    $runlist =~ s/\s|_//g;
    return if $runlist eq $self->EMPTY_STRING;

    my @ranges;

    for my $run ( split ",", $runlist ) {
        if ( $run =~ /^ (-?\d+) $/x ) {
            push @ranges, ( $1, $1 );
        }
        elsif ( $run =~ /^ (-?\d+) - (-?\d+) $/x ) {
            confess "Bad order: $runlist\n" if $1 > $2;
            push @ranges, ( $1, $2 );
        }
        else {
            confess "Bad syntax: $runlist\n";
        }
    }

    return @ranges;
}

# Converts a set specification into a set
sub _real_set {
    my $self     = shift;
    my $supplied = shift;

    if ( defined $supplied and ref $supplied eq ref $self ) {
        return $supplied;
    }
    else {
        return blessed($self)->new($supplied);
    }
}

# Return the index of the first element >= the supplied value.
#
# If the supplied value is larger than any element in the list the returned
# value will be equal to the size of the list.
#
# If $pos & 1, i.e. $pos is odd number, $val is in the set
sub _find_pos {
    my $self = shift;
    my $val  = shift;
    my $low  = shift;

    my $edges_ref = $self->edges_ref;
    my $high      = $self->edge_size;

    while ( $low < $high ) {
        my $mid = int( ( $low + $high ) / 2 );
        if ( $val < $edges_ref->[$mid] ) {
            $high = $mid;
        }
        elsif ( $val > $edges_ref->[$mid] ) {
            $low = $mid + 1;
        }
        else {
            return $mid;
        }
    }

    return $low;
}

=method B<INTERFACE: Aliases>

    runlist, run_list           => as_string

    elements                    => as_array

    size, count                 => cardinality

    empty                       => is_empty

    contains, contain, member   => contains_all

    duplicate                   => copy

    intersection                => intersect

    equals                      => equal

    lookup_index                => at

    lookup_member               => index

    join_span                   => fill

=cut

sub runlist       { shift->as_string(@_); }
sub run_list      { shift->as_string(@_); }
sub elements      { shift->as_array(@_); }
sub size          { shift->cardinality(@_); }
sub count         { shift->cardinality(@_); }
sub empty         { shift->is_empty; }
sub contains      { shift->contains_all(@_); }
sub contain       { shift->contains_all(@_); }
sub member        { shift->contains_all(@_); }
sub duplicate     { shift->copy; }
sub intersection  { shift->intersect(@_); }
sub equals        { shift->equal(@_); }
sub lookup_index  { shift->at(@_); }
sub lookup_member { shift->index(@_); }
sub join_span     { shift->fill(@_); }

1;    # Magic true value required at end of module

__END__

=head1 SYNOPSIS

    use AlignDB::IntSpan;

    my $set = AlignDB::IntSpan->new;
    $set->add(1, 2, 3, 5, 7, 9);
    $set->add_range(100, 1_000_000);
    print $set->as_string, "\n";    # 1-3,5,7,9,100-1000000

=head2 Operator overloads

    if ($set) { ... }   # true if $set is not empty

    print "$set\n";     # stringizes to the run list

=head1 DESCRIPTION

The C<AlignDB::IntSpan> module represents sets of integers as a number of
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
C<$NEG_INF> and C<$POS_INF> are defined to be -(2^31-1) and (2^31-2)
respectively. To create an infinite set invert an empty one:

    my $inf = AlignDB::IntSpan->new->complement;

Sets need only be bounded in one direction - for example this is the set of
all positive integers (assuming you accept the slightly feeble definition of
infinity we're using):

    my $pos_int = AlignDB::IntSpan->new;
    $pos_int->add_range(1, $pos_int->POS_INF);

Many codes come from L<Set::IntSpan>, L<Set::IntSpan::Fast> and
L<Set::IntSpan::Island>.

=cut

