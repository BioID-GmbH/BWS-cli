    token 123 enroll
    enroll enroll?.png
    pause
    token 123 identify livedetection-
    identify current.jpg

This enrolls all PNG files matching the pattern to a [BCID](./bcid.md) ending in 123. After
giving the BWS a little time to think, and an identification is done with
another image.

---

    token 123 verify
    verify "other person?.png"

    # The verification should fail.
    expect 1

    token 123 verify
    verify correct-person?.png

[Verify](./verify.md) images against the 123 BCID. We expect that this verification fails and
continue only in that case. After that another person is checked and the result
is returned to the shell.

---

Back to [TOC](./toc.md)