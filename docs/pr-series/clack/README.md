# Separate Clack shutdown PR

Upstream base: `9435762a8fc139edde8c682502a037332b0655f8`.
Topic head: `3fbf148681d559a5aea62f1b184516441fc336f6`.

[PR description](PR.md), [mail patch](threaded-stop.patch),
[Git bundle](threaded-stop.bundle) and [test inventory](manifest.json).
The patch preserves a bounded fallback when no graceful hook exists.
Two focused tests cover the graceful hook/natural unwind and missing-hook
fallback; twelve live Woo request/stop cycles passed at fork source `4d005a6`.
That Woo runtime is byte-identical to the assembled Woo stack.

To import into a clone with the base commit:

```sh
git fetch /absolute/path/to/threaded-stop.bundle codex/threaded-stop:codex/threaded-stop
git switch codex/threaded-stop
```

The prepared local branch is `/tmp/clack-pr-threaded-stop` and is pushed to
[`pyrex41/clack:codex/threaded-stop`](https://github.com/pyrex41/clack/tree/codex/threaded-stop).
[Review the exact delta](https://github.com/pyrex41/clack/compare/9435762a8fc139edde8c682502a037332b0655f8...3fbf148681d559a5aea62f1b184516441fc336f6).
The bundle also preserves the commit for offline import. No upstream PR was published.
