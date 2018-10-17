Get a token for a biometric operation for a mandatory [BCID](./bcid.md). Images uploaded
after this call but before the biometric call are considered to belong together.

Example call: `token bws.123.456 task=enroll`


Optional parameters:

    livedetection{+|-}   Enable or disable live detection (2+ images needed).

                         Attention: Disabling will have no effect if it was
                         enabled for the token!

    task=verify          Which task shall the token be for? Possible choices are
                         verify, enroll, identify and livenessdetection.

    maxtries=3           How many retries are possible (max 15).

    challenge{+|-}       Whether challenge response shall be enabled.

                         Attention: Disabling here cannot override an enable in
                         the users personal settings!

    challenges=3         How many challeges shall be required (2 to 7).

    autoenroll{+|-}      If set to true (default: false) verification images are
                         also used for enrollment to keep the template up to
                         date. This of course only happens for successful
                         verifications.

    traits=face,periocular Which traits shall be used?

---

Back to [TOC](./toc.md)