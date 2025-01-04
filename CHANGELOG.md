## Next

- Support non-ASCII UTF-8 in `Subject` and other unstructured
  headers.  ([#82])

- Support non-ASCII UTF-8 in display name in the `Data.IMF.Text`
  parser (the `ByteString` parser still only supports non-ASCII
  input via encoded-word).  ([#87])

[#82]: https://github.com/purebred-mua/purebred-email/issues/82
[#87]: https://github.com/purebred-mua/purebred-email/issues/87


## Version 0.6 (2022-09-13)

- Parameterise the `ContentType` data type over the "parameters"
  field.  The type becomes `ContentTypeWith a`.  The *data
  constructor* name remains `ContentType`.  Reintroduce
  `ContentType` as a *type synonym* for `ContentTypeWith
  Parameters`.  A "bare content type" without parameters can be
  represented as `ContentTypeWith ()`.

- Introduce `emptyParameters :: Parameters`.  It is the same as
  `mempty` but is convenient where `mempty` is ambiguous.

- Accept `Content-Type: multipart/related` without `type` parameter.

  [RFC 2387][] requires the `type` parameter.  Nevertheless some
  producers, including GMail and Fastmail, generate non-conformant
  messages without the `type` parameter.  Wrap the `Related`
  constructor's `ContentType` field in a `Maybe`. ([#68][])

- Introduce the `ContentID` type and use it for the
  `multipart/related` `start` parameter.

- Add missing end-of-input assertions to several "secondary"
  parsers, which could otherwise accept invalid data.

- `defaultCharsets`: recognise `ascii` as an alias of `us-ascii`.
  ([#69][])


[RFC 2387]: https://datatracker.ietf.org/doc/html/rfc2387#section-3.1
[#68]: https://github.com/purebred-mua/purebred-email/issues/68
[#69]: https://github.com/purebred-mua/purebred-email/issues/69


## Older versions

See Git commit history
