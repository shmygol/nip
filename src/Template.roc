module [parse_template]

import Match exposing [Spec, Matcher]
import Interval exposing [Interval]

parse_field_name : List U8 -> [Anonymous, Named Str]
parse_field_name = |field_name|
    when field_name is
        ['_'] | [] -> Anonymous
        _ -> field_name |> Str.from_utf8_lossy |> Named

expect
    # that `parse_field_name` returns `Anonymous`
    # when the field name is "_"
    field_name = ['_']
    actual = parse_field_name(field_name)
    actual == Anonymous

expect
    # that `parse_field_name` returns `Anonymous`
    # when the field name is an empty string
    field_name = []
    actual = parse_field_name(field_name)
    actual == Anonymous

expect
    # that `parse_field_name` returns `Named` with the field name
    # when the field name is a string, e.g. "field1"
    field_name = ['f', 'i', 'e', 'l', 'd', '1']
    actual = parse_field_name(field_name)
    actual == Named("field1")

## Parses a literal segment (a list of bytes)
## into a `Spec` with a `Match.literal` matcher.
parse_literal_segment : List U8 -> Spec _
parse_literal_segment = |segment| {
    field_name: Anonymous,
    length: segment |> List.len |> Interval.exact,
    matcher: segment |> Match.literal,
}

## Parses a token length (a list of bytes) into a `U64`.
## Returns `Err` if the token length is not valid, e.g. not a number.
parse_length : List U8 -> Result (Interval Unsigned64) [InvalidTokenLength Str]
parse_length = |length_str|
    length_utf8 =
        length_str
        |> Str.from_utf8_lossy

    length_utf8
    |> Str.to_u64
    |> Result.map_err(|_| InvalidTokenLength(length_utf8))?
    |> Interval.exact
    |> Ok

expect
    # that `parse_length` returns `Ok` with the length
    # when the length is a valid number
    length_str = ['1', '2', '3']
    actual = parse_length(length_str)
    actual == Ok(Interval.exact(123))

expect
    # that `parse_length` returns `Err(InvalidTokenLength)`
    # when the length is an empty string
    length_str = []
    actual = parse_length(length_str)
    actual == Err(InvalidTokenLength(""))

expect
    # that `parse_length` returns `Err(InvalidTokenLength)`
    # when the length is not a valid number
    length_str = ['a', 'b', 'c']
    actual = parse_length(length_str)
    actual == Err(InvalidTokenLength("abc"))

## Parses a token type (a list of bytes) into a `Matcher` function:
## - `s` -> `Match.anything`
##
## Returns `Err` if the token type is not valid.
parse_token_type : List U8 -> Result (Matcher _) [InvalidTokenType Str]
parse_token_type = |token_type|
    when token_type is
        ['*'] -> Ok(Match.anything)
        ['s'] -> Ok(Match.word)
        ['w'] -> Ok(Match.whitespace)
        ['i'] -> Ok(Match.signed_integer)
        ['u'] -> Ok(Match.unsigned_integer)
        ['d'] -> Ok(Match.decimal)
        ['[', '^', .. as allowed_bytes, ']'] -> Ok(Match.negated_charset(allowed_bytes))
        ['[', .. as allowed_bytes, ']'] -> Ok(Match.charset(allowed_bytes))
        _ -> token_type |> Str.from_utf8_lossy |> InvalidTokenType |> Err

## Parses a token (a list of bytes without curly braces) into a `Spec`.
## Returns `Err` if the token is not valid, i.e.:
## - TooManyTokenParts for more than 3 parts devided by `:`
## - InvalidTokenLength for invalid length, e.g. not a number
## - InvalidTokenType for invalid token type
parse_token : List U8 -> Result (Spec _) [TooManyTokenParts Str, InvalidTokenLength Str, InvalidTokenType Str]
parse_token = |token|
    when List.split_on(token, ':') is
        [] ->
            Ok(
                {
                    field_name: Anonymous,
                    length: Interval.all,
                    matcher: Match.anything,
                },
            )

        [field_name] ->
            Ok(
                {
                    field_name: parse_field_name(field_name),
                    length: Interval.all,
                    matcher: Match.anything,
                },
            )

        [field_name, token_type] ->
            Ok(
                {
                    field_name: parse_field_name(field_name),
                    length: Interval.all,
                    matcher: parse_token_type(token_type)?,
                },
            )

        [field_name, token_type, length] ->
            Ok(
                {
                    field_name: parse_field_name(field_name),
                    length: parse_length(length)?,
                    matcher: parse_token_type(token_type)?,
                },
            )

        _ -> token |> Str.from_utf8_lossy |> TooManyTokenParts |> Err

## Internal helper function,
## that returns a first segment and the rest of a given template.
## The segment is a token enclosed in curly braces `{}` or a literal string between tokens.
split_first_segment : List U8 -> Result { before : List U8, others : List U8 } [ListWasEmpty, MissingClosingBracket]
split_first_segment = |template_bytes|
    when template_bytes is
        [] ->
            Err(ListWasEmpty)

        ['{', ..] ->
            template_bytes
            |> List.find_first_index(|elem| elem == '}')
            |> Result.map_both(
                |index| List.split_at(template_bytes, index + 1),
                |_| MissingClosingBracket,
            )

        _ ->
            template_bytes
            |> List.find_first_index(|elem| elem == '{')
            |> Result.map_ok(|index| List.split_at(template_bytes, index))
            |> Result.with_default({ before: template_bytes, others: [] })
            |> Ok

expect
    # that `split_first_segment` returns `Err(ListWasEmpty)`
    # when the template bytes is an empty list
    actual = split_first_segment([])
    actual == Err(ListWasEmpty)

expect
    # that `split_first_segment` returns a segment and the rest of the input
    # when the template starts with a token enclosed in curly braces
    actual = split_first_segment(
        ['{', 'f', 'i', 'e', 'l', 'd', '1', '}', 'x', '{', 'x', '}'],
    )
    actual
    == Ok(
        {
            before: ['{', 'f', 'i', 'e', 'l', 'd', '1', '}'],
            others: ['x', '{', 'x', '}'],
        },
    )

expect
    # that `split_first_segment` returns a segment and the rest of the input
    # when the template starts with a literal string
    actual = split_first_segment(
        ['t', 'e', 'x', 't', '{', 'x', '}', '{', 'y', '}'],
    )
    actual
    == Ok(
        {
            before: ['t', 'e', 'x', 't'],
            others: ['{', 'x', '}', '{', 'y', '}'],
        },
    )

expect
    # that `split_first_segment` returns `Err(MissingClosingBracket)`
    # when the template starts with a token enclosed in curly braces
    # but the closing bracket is missing
    actual = split_first_segment(
        ['{', 'f', 'i', 'e', 'l', 'd', '1'],
    )
    actual == Err(MissingClosingBracket)

expect
    # that `split_first_segment` returns an entire template as a segment
    # when the template is a single token enclosed in curly braces
    actual = split_first_segment(
        ['{', 'f', 'i', 'e', 'l', 'd', '1', '}'],
    )
    actual
    == Ok(
        {
            before: ['{', 'f', 'i', 'e', 'l', 'd', '1', '}'],
            others: [],
        },
    )

expect
    # that `split_first_segment` returns an entire template as a segment
    # when the template is a single literal string
    actual = split_first_segment(
        ['t', 'e', 'x', 't'],
    )
    actual
    == Ok(
        {
            before: ['t', 'e', 'x', 't'],
            others: [],
        },
    )

## See `parse_template` for the public API.
parse_template_recursive : List U8, List (Spec _) -> Result (List (Spec _)) [MissingClosingBracket, TooManyTokenParts Str, InvalidTokenLength Str, InvalidTokenType Str]
parse_template_recursive = |template_bytes, acc|
    when split_first_segment(template_bytes) is
        Err(ListWasEmpty) -> Ok(acc)
        Err(MissingClosingBracket) -> MissingClosingBracket |> Err
        Ok({ before: ['{', .. as token, '}'], others }) -> parse_template_recursive(others, List.append(acc, parse_token(token)?))
        Ok({ before, others }) -> parse_template_recursive(others, List.append(acc, parse_literal_segment(before)))

## Parses a template string into a list of `Spec`s
## Template string can contain:
## - literal strings between tokens
## - tokens enclosed in curly braces `{}` with the following format:
##   `{field_name}`
##   or `{field_name:token_type}`
##   or `{field_name:token_type:length}`
##
## Examples:
## - `"{}"` -> not implemented yet
##   ```
##   [
##     {
##       field_name: Anonymous,
##       length: Interval.all,
##       matcher: Match.anything
##     }
##   ]
##   ```
## - `"{field1}"` -> not implemented yet
##   ```
##   [
##     {
##       field_name: "field1",
##       length: Interval.all,
##       matcher: Match.anything
##     }
##   ]
##   ```
## - `"{field1:s}"` -> not implemented yet
##   ```
##   [
##     {
##       field_name: "field1",
##       length: Interval.all,
##       matcher: Match.anything
##     }
##   ]
##   ```
## - `"{field1:s:6}text"` ->
##   ```
##   [
##     {
##       field_name: Named "field1",
##       length: Interval.exact(6),
##       matcher: Match.anything
##     }, {
##       field_name: Anonymous,
##       length: Interval.exact(4),
##       matcher: Match.literal(['t', 'e', 'x', 't'])
##     }
##   ]
##   ```
parse_template : Str -> Result (List (Spec _)) [MissingClosingBracket, TooManyTokenParts Str, InvalidTokenLength Str, InvalidTokenType Str]
parse_template = |template_str|
    template_str
    |> Str.to_utf8
    |> parse_template_recursive([])
