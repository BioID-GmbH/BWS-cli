Upload files to the BWS for a biometric operation to come. Provide one or more
file paths.

Example call: `upload left.png tag=left index=0`

Optional parameters:  
   tag                  Attach some info/description to the uploaded file. This
                        can e.g. be used with challenge response to say which
                        movement an image was meant for (i.e. left, right, up,
                        down).

   index                An index for a sequence of images. This can be necessary
                        because images might not arrive in the order they were
                        sent. This can then mess up challenge response.

   trait                Which trait shall be used?

---

Back to [TOC](./toc.md)