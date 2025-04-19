## Nip is a simple string parser library for Roc,
## using templates with fields like `"Hello, {salutation}"`.
module [match_all]

import Template exposing [parse_template]
import Match

## Attempts to match a template string against an input string.
## Returns a dictionary with named fields and their values,
## or an error if the template does not match the input.
##
## A template can contain fields in the following format:
##
## ```
## {name:type:length}
## ```
##
## - `name` - a valid field name in Roc or `"_"` to match the spec but ignore the value
## - `type` - an optional type of a matcher for the field
## - `length` - an optional length for the value to match
##
## Supported specs:
##
## - `s` - Default spec. Matches any value.
##
## Returns following errors:
## - `Err DoesNotMatch` if the template does not match the input.
## - `Err TemplateError Reason` if the template is invalid.
##   See [Template.parse_template] for reasons.
match_all :
    Str,
    Str
    -> Result
        (Dict Str Str)
        [
            DoesNotMatch,
            TemplateError [
                    MissingClosingBracket,
                    InvalidTokenLength Str,
                    InvalidTokenType Str,
                    TooManyTokenParts Str,
                ],
        ]
match_all = |template_str, input_str|
    specs =
        template_str
        |> parse_template
        |> Result.map_err(TemplateError)?

    input_str
    |> Str.to_utf8
    |> Match.all_specs(specs)
