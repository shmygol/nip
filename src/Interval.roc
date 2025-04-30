## Intervals are used to express constraints like minimum and maximum lengths.
module [
    Interval,
    exact,
    at_least,
    at_most,
    all,
    bounded_within,
    BoundedInterval,
    contains_strict,
    to_list,
    split_head,
    width,
    is_empty,
    is_within,
    map,
    walk,
    walk_until,
]

## An interval with potentially unbounded ends.
## Use `bounded_within` to convert it to a bounded interval.
## For simplicity `Interval` is implemented as a type alias,
## thus if `from` is greater than `to`, the interval is considered empty.
Interval int_type_variable : {
    from : [
        At (Int int_type_variable),
        After (Int int_type_variable),
        Unbound,
    ],
    to : [
        At (Int int_type_variable),
        Before (Int int_type_variable),
        Unbound,
    ],
}

## Returns an interval with a given value at both ends.
exact : Int a -> Interval a
exact = |value|
    { from: At(value), to: At(value) }

## Returns an interval with a given lower bunded and unbounded upper bound.
at_least : Int a -> Interval a
at_least = |value|
    { from: At(value), to: Unbound }

## Returns an interval with a given upper bunded and unbounded lower bound.
at_most : Int a -> Interval a
at_most = |value|
    { from: Unbound, to: At(value) }

## Returns an universal interval, i.e. unbounded on both sides.
all : Interval _
all = { from: Unbound, to: Unbound }

## Converts a potentially unbounded interval to a bounded one,
## by replacing `Unbound` with the given `lowest` and `highest` values.
##
## Returns `Err(OutOfBounds)` if any part of the interval falls outside.
##
## Note, that for example `After 1` is allowed with `lowest` of `2`,
bounded_within : Interval a, Int a, Int a -> Result (BoundedInterval a) [OutOfBounds]
bounded_within = |{ from, to }, lowest, highest|
    bounded_interval = {
        from:
        when from is
            At(value) -> At(value)
            After(value) -> After(value)
            Unbound -> At(lowest),
        to:
        when to is
            At(value) -> At(value)
            Before(value) -> Before(value)
            Unbound -> At(highest),
    }

    if is_within(bounded_interval, { from: At(lowest), to: At(highest) }) then
        Ok(bounded_interval)
    else
        Err(OutOfBounds)

expect
    # that `bounded_within` converts an unbounded interval to a bounded one
    interval : Interval Unsigned64
    interval = { from: Unbound, to: Unbound }

    actual = bounded_within(interval, 1, 10)
    actual == Ok({ from: At(1), to: At(10) })

expect
    # that `bounded_within` returns the same interval
    # when it's a factually bounded one
    # and within the given lowest and highest values
    interval : Interval Unsigned64
    interval = { from: At 1, to: At 10 }

    actual = bounded_within(interval, 0, 15)
    actual == Ok({ from: At 1, to: At 10 })

expect
    # that `bounded_within` returns the same interval
    # when the interval is empty
    interval : Interval Unsigned64
    interval = { from: At 10, to: At 1 }

    actual = bounded_within(interval, 0, 1000)
    actual == Ok({ from: At 10, to: At 1 })

expect
    # that `bounded_within` returns an ok result
    # when the lowest and highest values are at the ends of the interval
    # (with a special case for exclusive intervals)
    interval : Interval Unsigned64
    interval = { from: After 1, to: At 10 }

    actual = bounded_within(interval, 2, 10)
    actual == Ok({ from: After 1, to: At 10 })

expect
    # that `bounded_within` returns an error
    # when the interval is not within the given lowest and highest values
    interval : Interval Unsigned64
    interval = { from: At 1, to: At 10 }

    actual = bounded_within(interval, 2, 8)
    actual == Err(OutOfBounds)

expect
    # that `bounded_within` returns an `OutOfBounds` error
    # when the lowest is greater than the highest value
    interval : Interval Unsigned64
    interval = { from: At 5, to: At 5 }

    actual = bounded_within(interval, 8, 2)
    actual == Err(OutOfBounds)

expect
    # that `bounded_within` always returns an `OutOfBounds` error
    # when the lowest is greater than the highest value
    # even the from value is greater than the lowest value
    # and the to value is less than the highest value
    interval : Interval Unsigned64
    interval = { from: At 9, to: At 1 }

    actual = bounded_within(interval, 8, 2)
    actual == Err(OutOfBounds)

## A bounded interval with both ends bounded.
## Similar to [Interval], `BoundedInterval` is implemented as a type alias,
## thus if `from` is greater than `to`, the interval is considered empty.
BoundedInterval int_type_variable : {
    from : [At (Int int_type_variable), After (Int int_type_variable)],
    to : [At (Int int_type_variable), Before (Int int_type_variable)],
}

## Internal helper function, that
## returns a tuple of the normalized inclusive bounds of the interval
## if the interval is not empty,
## or an `IntervalWasEmpty` error otherwise.
normalized : BoundedInterval a -> Result (Int a, Int a) [IntervalWasEmpty]
normalized = |{ from, to }|
    first =
        when from is
            At(value) ->
                value

            After(value) ->
                value + 1
    last =
        when to is
            At(value) ->
                value

            Before(value) ->
                value
                |> Num.sub_checked(1)
                |> Result.map_err(|_| IntervalWasEmpty)?

    if first > last then
        Err(IntervalWasEmpty)
    else
        Ok((first, last))

expect
    # that `normalized` returns the bounds of the interval
    # when the interval is inclusive
    interval : BoundedInterval Unsigned64
    interval = { from: At(10), to: At(20) }

    actual = normalized(interval)
    actual == Ok((10, 20))

expect
    # that `normalized` returns the bounds of the interval
    # when the interval is exclusive
    interval : BoundedInterval Unsigned64
    interval = { from: At(10), to: Before(20) }

    actual = normalized(interval)
    actual == Ok((10, 19))

expect
    # that `normalized` returns the bounds saturated
    # when the interval is of unsigned type
    # and the to value is unsigned `Before(0)`
    # instead of crashing with an underflow
    interval : BoundedInterval Unsigned64
    interval = { from: At(10), to: Before(0) }

    actual = normalized(interval)
    actual == Err(IntervalWasEmpty)

## Returns `Bool.true` if a value is within the bounded interval,
## `Bool.false` otherwise.
contains_strict : BoundedInterval a, Int a -> Bool
contains_strict = |interval, value|
    when normalized(interval) is
        Err(IntervalWasEmpty) ->
            Bool.false

        Ok((first, last)) ->
            value >= first and value <= last

expect
    # that `contains` returns `true`
    # when the value is within the bounded interval
    interval : BoundedInterval Unsigned64
    interval = { from: At(1), to: At(10) }

    actual = contains_strict(interval, 5)
    actual == Bool.true

expect
    # that `contains` returns `false`
    # when the value is outside the bounded interval
    interval : BoundedInterval Unsigned64
    interval = { from: At(1), to: At(10) }

    actual = contains_strict(interval, 11)
    actual == Bool.false

expect
    # that `contains` returns `false`
    # when the value is on the exclusive left bound
    interval : BoundedInterval Unsigned64
    interval = { from: After(1), to: At(10) }

    actual = contains_strict(interval, 1)
    actual == Bool.false

expect
    # that `contains` returns `false`
    # when the interval is empty
    interval : BoundedInterval Unsigned64
    interval = { from: After(10), to: At(1) }

    actual = contains_strict(interval, 15)
    actual == Bool.false

## Returns a list of values from the bounded interval.
to_list :
    BoundedInterval a
    -> List (Int a)
to_list = |{ from, to }|
    List.range(
        {
            start: from,
            end:
            when to is
                # convert `to` from `[Before, At]` to `[Before, At, Length]`
                At(value) -> At(value)
                Before(value) -> Before(value),
            step: 1,
        },
    )

expect
    # that `to_list` returns a list of values from the bounded interval
    interval : BoundedInterval Unsigned64
    interval = { from: At(1), to: Before(5) }

    actual = to_list(interval)
    actual == [1, 2, 3, 4]

expect
    # that `to_list` returns an empty list
    # when the interval is empty
    interval : BoundedInterval Unsigned64
    interval = { from: At(10), to: Before(0) }

    actual = to_list(interval)
    actual == []

## Returns the width of the bounded interval,
## i.e. the number of values in the interval.
width : BoundedInterval a -> U64
width = |interval|
    when normalized(interval) is
        Err(IntervalWasEmpty) -> 0
        Ok((first, last)) -> Num.int_cast(last - first + 1)

## Returns `Bool.true` if the interval is empty,
## `Bool.false` otherwise.
## An interval is empty if the first value is greater than the last one.
is_empty : BoundedInterval _ -> Bool
is_empty = |interval|
    normalized(interval) == Err(IntervalWasEmpty)

## Returns `Bool.true` if the first interval is within the second one,
## `Bool.false` otherwise.
## If the inner interval is empty, it is always within the outer one,
## unless the outer interval is also empty.
## An empty interval can never contain another interval.
is_within : BoundedInterval _, BoundedInterval _ -> Bool
is_within = |inner_interval, outer_interval|
    when (normalized(inner_interval), normalized(outer_interval)) is
        (_, Err(IntervalWasEmpty)) ->
            Bool.false

        (Err(IntervalWasEmpty), _) ->
            Bool.true

        (Ok((inner_first, inner_last)), Ok((outer_first, outer_last))) ->
            inner_first >= outer_first and inner_last <= outer_last

        _ ->
            crash(
                """
                Should not be needed, but the compiler has a bug: 
                https://github.com/roc-lang/roc/issues/5530
                """,
            )

## Returns the first value of the bounded interval as `head`
## and a new bounded interval starting from the next value as `tail`.
split_head : BoundedInterval a -> Result { head : Int a, tail : BoundedInterval a } [IntervalWasEmpty]
split_head = |interval|
    interval
    |> normalized?
    |> .0
    |> |first| {
        head: first,
        tail: { from: At(first + 1), to: interval.to },
    }
    |> Ok

## Maps a function over the values in the bounded interval.
map : BoundedInterval a, (Int a -> result) -> List result
map = |interval, f|
    walk(
        interval,
        [],
        |state, element|
            List.append(
                state,
                f(element),
            ),
    )

expect
    # that `map` returns a list of values from the bounded interval
    interval : BoundedInterval Unsigned64
    interval = { from: At(1), to: At(5) }

    actual = map(interval, |x| x * 2)
    actual == [2, 4, 6, 8, 10]

expect
    # that `map` returns a list of values from the bounded exclusive interval
    interval : BoundedInterval Unsigned64
    interval = { from: After(1), to: At(5) }

    actual = map(interval, |x| x * 2)
    actual == [4, 6, 8, 10]

expect
    # that `map` returns a list of values from the bounded exclusive interval
    interval : BoundedInterval Unsigned64
    interval = { from: At(1), to: Before(5) }

    actual = map(interval, |x| x * 2)
    actual == [2, 4, 6, 8]

expect
    # that `map` returns an empty list
    # when the interval is empty
    interval : BoundedInterval Unsigned64
    interval = { from: After(5), to: Before(5) }

    actual = map(interval, |x| x * 2)
    actual == []

expect
    # that `map` returns an empty list
    # when the interval is empty
    # even for the unsigned integers
    interval : BoundedInterval Unsigned64
    interval = { from: After(5), to: Before(1) }

    actual = map(interval, |x| x * 2)
    actual == []

## Returns a new state after processing all values in the bounded interval
walk : BoundedInterval a, state, (state, Int a -> state) -> state
walk = |interval, state, f|
    when split_head(interval) is
        Err(IntervalWasEmpty) ->
            state

        Ok({ head, tail }) ->
            walk(
                tail,
                f(state, head),
                f,
            )

expect
    # that `walk` returns the final state after processing all values in the bounded interval
    interval : BoundedInterval Unsigned64
    interval = { from: At(1), to: At(5) }

    actual = walk(interval, 0, Num.add)
    actual == 15

expect
    # that `walk` returns the final state after processing all values in the bounded exclusive interval
    interval : BoundedInterval Unsigned64
    interval = { from: After(1), to: At(5) }

    actual = walk(interval, 0, Num.add)
    actual == 14

expect
    # that `walk` returns the final state after processing all values in the bounded exclusive interval
    interval : BoundedInterval Unsigned64
    interval = { from: At(1), to: Before(5) }

    actual = walk(interval, 0, Num.add)
    actual == 10

## Same as [walk], except you can step walking early.
walk_until :
    BoundedInterval a,
    state,
    (state, Int a -> [Continue state, Break state])
    -> state
walk_until = |interval, state, f|
    when split_head(interval) is
        Err(IntervalWasEmpty) ->
            state

        Ok({ head, tail }) ->
            when f(state, head) is
                Continue(new_state) ->
                    walk_until(tail, new_state, f)

                Break(final_state) ->
                    final_state

expect
    # that `walk_until` returns the final state after processing all values
    # when the function returns `Continue`
    interval : BoundedInterval Unsigned64
    interval = { from: At(1), to: At(5) }

    actual = walk_until(interval, 0, |state, x| Continue(Num.add(state, x)))
    actual == 15

expect
    # that `walk_until` returns the final state after processing values up to the first `Break`
    interval : BoundedInterval Unsigned64
    interval = { from: At(1), to: At(20) }

    actual = walk_until(
        interval,
        0,
        |state, x|
            if x < 10 then Continue(Num.add(state, x)) else Break(state),
    )
    actual == 45

expect
    # that `walk_until` returns the initial state
    # when the interval is empty
    interval : BoundedInterval Unsigned64
    interval = { from: After(100), to: Before(0) }

    actual = walk_until(
        interval,
        "",
        |state, x|
            Continue("${state} ${Num.to_str(x)}"),
    )
    actual == ""
