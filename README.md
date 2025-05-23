# nip

Nip is a simple string parser library for Roc, using templates with fields like `"Hello, {salutation}"`.
Currently Nip doesn't use `Decode` and returns a dict with values of type `Str` recardless of the matcher type, which is enough for my use case, but can be extended to return a record.

## Fields syntax

A template can contain fields in the following format:

```
{name:type:length}
```

Where: 

- `name` - a valid field name in Roc or `"_"` to match the spec but not return the value.
- `type` - an optional type of a matcher for the field. Currently all matchers return a string.
- `length` - an optional length for the value to match

All segments of the token are optional, for example:

- `{}` - match any value and ignore it.
- `{name}` - match any value and return it under the given field name.
- `{name:type}` - match value of a given type and return it as a string under the given field name.
- `{name:type:length}` - match value of a given type of exact length and return it as a string a string under the given field name.
- `{_:type}`/`{_:type:length}` - it's also possible to omit the name to match the spec but not return the value.

Supported types:

- `*` - Default type. Matches any value.
- `s` - Match all non-whitespace characters.
- `w` - Whitespace(s).
- `i` - Integer optionally preceded by a sign.
- `u` - Unsigned integer.
- `d` - Decimal optionally preceded by a sign.
- `[characters]` - Characters specified between the brackets.
- `[^characters]` - Any characters except for the specified  between the brackets.

# Known issues

- https://github.com/shmygol/nip/issues/8: ⚠️ Unfortunately, the library is not usable because of a compiler bug
