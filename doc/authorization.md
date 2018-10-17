There are two ways to be authorized for BWS API calls:
1. Basic HTTP Authentication Scheme (RFC 7617)
2. JSON Web Tokens (RFC 7519)

The second method is used for the four core biometric operations: [enroll](./enroll.md),
[verify](./verify.md), [identify](./identify.md) and [livenessdetection](./livenessdetection.md) (plus [upload](./upload.md)). To use these you
first need to get a JWT via the [token](./token.md) call. Other stateless calls use the
first method.

For both ways, you need to be registered and have been provided with an
endpoint, an application ID and an application password. These can be passed as
[tool](./tool.md) arguments and the rest is done automatically.

---

Back to [TOC](./toc.md)