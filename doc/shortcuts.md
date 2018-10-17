All basic commands can be abbreviated as long as the abbreviation is not
ambiguous.

Examples: is->isenrolled, id->identify, q->qualitycheck

Whenever a [BCID](./bcid.md) shall be provided, you can just use a single number. Because
the tool is given a storage and a partition, it will assemble a valid BCID
automatically.

Example: isenrolled 456 face -> isenrolled bws.123.456 face

For the [token](./token.md) command the task can be given abbreviated.

Example: token 456 e -> token bws.123.456 task=enroll

After getting a token, normally one would issue [upload](./upload.md) commands which are then
followed by a biometric command. You can leave out upload and just give the file
names to the biometric command and the rest is handled automatically.

Example: verify a.png b.png
      -> upload a.png
         upload b.png
         verify

All commands that take a file accept multiple files in one call as well as glob
patterns using ? and *.

Example call: `enroll face*.png eye?.png`

All boolean parameters need not be given like setting=true. The shorthand
setting+ or setting- is understood.

Example call: `enroll livedetection+`

---

Back to [TOC](./toc.md)