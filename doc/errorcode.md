This is most useful for piping a file with commands into the [tool](./tool.md). When the
execution is finished, the first non-zero error code is returned to the shell.

These are the exit codes that are defined:  
0: Ok
1: OkButNo          Failed verification/livenessdetection or user unknown.
2: Unauthorized     Missing/wrong/stale authentication.
3: CommandError     Unknown command entered.
4: ParameterError   Command was recognized, but the parameters had errors.
5: BwsError         The BWS returned an error. See --print-body in <help tool>.

To get a success return code when you were expecting a non-zero code, see
[expect](./expect.md) command.

---

Back to [TOC](./toc.md)