# nip

Nip is a simple string parser library for Roc, using templates with fields like `"Hello, {salutation}"`.

## Fields syntax

A template can contain fields in the following format:

```
{name:type:length}
```

- `name` - a valid field name in Roc or `"_"` to match the spec but ignore the value
- `type` - an optional type of a matcher for the field
- `length` - an optional length for the value to match

Supported specs:

- `s` - Default spec. Matches any value.