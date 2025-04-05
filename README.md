# nip

Nip is a simple string parser library for Roc, using templates with fields like `"Hello, {salutation:Str}"`.

## Fields syntax

A can template can contain fields in the following format:

```
{name:spec}
```

- `name` - a valid field name in Roc
- `spec` - an optional specification for the field
