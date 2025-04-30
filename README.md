# nip

Nip is a simple string parser library for Roc, using templates with fields like `"Hello, {salutation}"`.
Currently Nip doesn't use `Decode` and returns a dict with values of type `Str` recardless of the matcher type, which is enough for my use case, but can be extended to return a record.

## Fields syntax

A template can contain fields in the following format:

```
{name:type:length}
```

Where: 

- `name` - a valid field name in Roc or `"_"` to match the spec but ignore the value
- `type` - an optional type of a matcher for the field. Currently all matchers return a string.
- `length` - an optional length for the value to match

All segments of the token are optional, for example:

- {} - match any value and ignore it.
- {name} - match any value and return it under the given field name.
- {name:type} - match value of a given type and return it as a string under the given field name.
- {name:type:length} - match value of a given type of exact length and return it as a string a string under the given field name.

Supported types:

- `s` - Default type. Matches any value.
- `i` - Integer optionally preceded by a sign.
- `u` - Unsigned integer.
- `d` - Decimal optionally preceded by a sign.
- `w` - Whitespace(s).
- `[characters]` - Characters specified between the brackets.
- `[^characters]` - Any characters except for the specified  between the brackets.