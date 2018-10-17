A BCID, or Biometric Class ID, is an identifier for the BWS. It consists of
three parts separated by a dot or a forward slash.

A single BCID must correspond to one and only one person. Never enroll different
people with the same BCID or use the same BCID for different people! Doing so
will severely degrade recognition.

We care about data privacy, so we don't want to know anything about your users.
You manage your users data with an associated BCID. And when you want to make a
biometric call, you provide the BCID and one ore more samples. We then tell you
the result of that biometric operation which you will handle in your system.

The first part of a BCID is the storage. This is a string that directly maps to
the Azure data center location where the template for this class is stored.

The second part is a partition. Typically you're assigned a partition and will
always use that number. Advanced use cases might need several partitions per
storage to its users in different groups. Note that an [identify](./identify.md) call only ever
looks at one specific partition.

The last part is again a number. There is no pattern is these numbers, just make
them up randomly as you enroll. But do take care to respect the one-to-one
mapping.

Example: `bws.123.456`
This identifies a class 456 within partition 123 of storage bws.

---

Back to [TOC](./toc.md)