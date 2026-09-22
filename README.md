# elogcat.el [![MELPA](http://melpa.org/packages/elogcat-badge.svg)](http://melpa.org/#/elogcat)

logcat interface for Emacs based on [android-mode](https://github.com/remvee/android-mode).

`elogcat-mode` is a read-only major mode derived from `special-mode`. It keeps a
bounded, structured backlog of `adb logcat -v threadtime` records. Filters and
minimum levels are applied to the retained backlog immediately, without
restarting adb. Pausing only stops rendering: incoming messages remain
available when the stream resumes. The mode line reports `LIVE`, `HOLD`, or
`PAUSED`, plus `WRAP` when soft wrapping is enabled.

Multi-line exceptions retain their header metadata, so tag, PID, and level
filters keep stack traces together. Error/Fatal/Assert headers and stack frames
can be browsed as one wrapping occurrence sequence.

`elogcat` starts with Android Studio's default `package:mine` query. When
android-mode knows the current module and variant, its application ID is used
automatically; otherwise press `P` to select the application represented by
`mine`. Package/process metadata is resolved client-side and follows app
restarts and remote processes such as `com.example.app:worker`. Clear the `/`
query to inspect all collected messages. Error, Fatal, and Assert messages
emitted by proxy processes such as `AndroidRuntime` are retained when their
complete message mentions the selected application.

Press `/` to enter an Android Studio compatible filter expression. Supported
fields are `tag:`, `package:`, `process:`, `message:`, and `line:`. Bare terms
search the complete formatted log entry. Terms support contains (`tag:foo`),
exact (`tag=:foo`), regex (`tag~:foo.*`), and negated variants such as
`-tag:foo` and `-tag~:foo.*`. Values may be single- or double-quoted.

Whitespace combines different fields with AND. Multiple positive terms for the
same field use Android Studio's implicit OR behavior. Explicit `&`, `|`, and
parentheses are supported, with `&` binding more tightly than `|`:

```text
tag:ActivityManager level:warn
package:mine (is:crash | is:stacktrace)
tag:Network | (process:worker & message~:"timeout|failed")
-package:com.example.debug age:10m
```

Special filters include `level:verbose|debug|info|warn|error|assert`,
`age:<number><s|m|h|d>`, `is:crash`, `is:stacktrace`, `is:firebase`, and exact
levels such as `is:error`. `package:mine` uses the package selected with `P`.
Invalid expressions fall back to a case-insensitive whole-line text search,
matching Android Studio's failure behavior. `M-c` toggles case-sensitive query
matching.

The `/` minibuffer provides context-sensitive completion through Emacs's
standard completion-at-point API. Press `TAB` to complete filter keys and
values. `level:`, `is:`, and `age:` use built-in candidates; `package:`, `tag:`,
and `process:` also offer values observed in the current backlog. This works
with standard completion and CAPF frontends such as Corfu. Minibuffer history
remains available with `M-p` and `M-n`.

## ScreenShot

- **elogcat**
<img align="center" src="https://raw.github.com/youngker/elogcat.el/master/elogcat.png">

## Installation

It's available on [Melpa](https://melpa.org/):

    M-x package-install elogcat

Requirements

- **adb**

[Installing the Android SDK](https://developer.android.com/sdk/installing/)

You can add these lines to your init file.

```elisp
(use-package elogcat
  :commands elogcat)
```


Key bindings

Key | Function
--- | --------
<kbd>SPC</kbd> | Pause/resume rendering while continuing to collect messages
<kbd>/</kbd> | Set or clear an Android Studio-compatible filter query
<kbd>l</kbd> | Select the minimum visible log level
<kbd>P</kbd> | Select the application represented by `package:mine`
<kbd>f</kbd> | Toggle follow-tail (`LIVE`/`HOLD`)
<kbd>w</kbd> | Toggle soft wrapping
<kbd>n</kbd> / <kbd>p</kbd> | Next/previous Error, Fatal, Assert, or stack frame
<kbd>c</kbd> | Clear the device log and local backlog
<kbd>o</kbd> | Run `occur`
<kbd>s</kbd> | Save the buffer and stop Logcat
<kbd>g</kbd> | Show detailed stream and filter status
<kbd>M-c</kbd> | Toggle case-sensitive query matching
<kbd>?</kbd> | Describe the mode and show all bindings
<kbd>q</kbd> | Stop Logcat and close the buffer

Legacy include/exclude regexp commands and ring-buffer toggle commands remain
available through `M-x`, but structured `/` queries are the primary filtering
interface. The stream collects Android's `main`, `system`, `radio`, `events`,
`crash`, and `kernel` ring buffers so diagnostic messages remain available to
those queries. Ring buffers are intentionally absent from the primary keymap
and status UI, matching modern Android Studio's app-and-query-centered
workflow.

## Configuration

```elisp
(setq elogcat-backlog-size (* 8 1024 1024) ; retained characters
      elogcat-soft-wrap t                   ; default for new buffers
      elogcat-default-tail 100              ; initial device history
      elogcat-default-query "package:mine"   ; Android Studio default
      elogcat-show-key-hints t                ; concise header shortcuts
      elogcat-process-refresh-interval 2)     ; package/PID refresh seconds
```

`elogcat-backlog-size` bounds the in-memory records and displayed output. Once
the limit is exceeded, the oldest complete records are discarded.

## License

Copyright (C) 2023 Youngjoo Lee

Author: Youngjoo Lee <youngker@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
