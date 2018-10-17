Checks the return code of the previous operation. If that matches the argument
of expect the execution is continued. If not processing is stopped and the last
return code is returned to the shell.

Example call: expect 1

This after a [verify](./verify.md) call means that we expect the verification to be not
successful.

Also if the expectation is true that return code is cleared, whatever it was
before. See [errorcode](./errorcode.md) for available numbers.

---

Back to [TOC](./toc.md)