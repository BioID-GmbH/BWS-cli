Remember a setting that is passed with all API calls. This is remembered until
the end of the session or until you clear it with [unset](./unset.md).

For instance, one might want to try things without live detection. Instead of
always passing it explicitly, one can set it once and it is always passed. Calls
that don't understand a parameter silently ignore it.

Example call: set livedetection=false bcid=bws.123.456

Settings are given one or more key=value strings. Don't put spaces around the
equal sign, it will confuse the parser!

Boolean settings can be specified using a trailing + or - for setting them to
true or false.

If you call set without arguments, the current list of settings is printed.

---

Back to [TOC](./toc.md)