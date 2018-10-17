Tries to find out if two live images match a picture that may come from an
identity document (e.g. passport).

The passport images needs to be the first parameter, two more provide the live
images.

Example call: `photoverify passport.png live1.png live2.png`


Optional parameters:

   accuracy=4           A confidence/strictness level from 1 to 5 (default: 4).
                        1 is not much better than guessing, not recommended!
                        5 is very strict and needs very good image quality.

---

Back to [TOC](./toc.md)