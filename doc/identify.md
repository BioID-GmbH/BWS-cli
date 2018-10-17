Identify which classes the uploaded images most likely belong to. The result is
a list of classes with decreasing confidence.

Example call: `identify maxresults=3`


Optional parameters:

    livedetection{+|-}   Enable or disable live detection (2+ images needed).

                         Attention: Disabling will have no effect if it was
                         enabled for the token!

    maxresults[=20]      The maximum number of matches to return.

---

Back to [TOC](./toc.md), ([official docs](https://developer.bioid.com/bwsreference/web-api/web-identify-api))