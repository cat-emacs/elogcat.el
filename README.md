# elogcat.el [![MELPA](http://melpa.org/packages/elogcat-badge.svg)](http://melpa.org/#/elogcat)

logcat interface for Emacs based on [android-mode](https://github.com/remvee/android-mode).

`elogcat-mode` is a read-only major mode derived from `special-mode`. It keeps a
bounded, structured backlog of `adb logcat -v threadtime` records. Filters and
minimum levels are applied to the retained backlog immediately, without
restarting adb. Pausing only stops rendering: incoming messages remain
available when the stream resumes. The mode line reports `CONNECTING`, `LIVE`,
`HOLD`, `PAUSED`, `RECONNECTING`, `OFFLINE`, or `ERROR`, plus `WRAP` when soft
wrapping is enabled. Unexpected disconnects use bounded exponential-backoff
reconnection; `r` reconnects immediately.

All adb operations are asynchronous and scoped to a buffer-local device serial.
A single connected device is selected automatically; when several are online,
`elogcat` prompts on startup. Press `D` to rediscover and switch devices.

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
remains available with `M-p` and `M-n`. Query fields and operators are
highlighted while editing. Invalid expressions still fall back to whole-line
matching, while the parser diagnostic remains visible in the Logcat header.
Queries can be recalled with `h`, saved with `C-c C-s`, and applied by name with
`N`.

Stack frames are source links: move to one and press `RET` (or middle-click) to
open the matching Kotlin or Java file under the project root at the referenced
line. `TAB` folds the current exception's stack frames and `S-TAB` toggles all
exception folds. Folding and display presets only redraw the local backlog.
Press `V` to choose Raw, Compact, Process, or Full fields.

## ScreenShot

- **elogcat**
<img align="center" src="https://raw.github.com/youngker/elogcat.el/master/elogcat.png">

## Installation

It's available on [Melpa](https://melpa.org/):

    M-x package-install elogcat

Requirements

- **adb**
- **Transient 0.3.0 or newer** (built into current Emacs releases)

[Installing the Android SDK](https://developer.android.com/sdk/installing/)

You can add these lines to your init file.

```elisp
(use-package elogcat
  :commands elogcat)
```


Key bindings

The `?` menu groups the primary command set while the same commands remain
available directly for established workflows.

Key | Function
--- | --------
<kbd>?</kbd> | Open the Logcat command menu
<kbd>SPC</kbd> | Pause/resume rendering while continuing to collect messages
<kbd>/</kbd> | Set or clear an Android Studio-compatible filter query
<kbd>l</kbd> | Select the minimum visible log level
<kbd>P</kbd> | Select the application represented by `package:mine`
<kbd>RET</kbd> | Open the source location referenced by a stack frame
<kbd>TAB</kbd> / <kbd>S-TAB</kbd> | Fold one exception / toggle all exception folds
<kbd>f</kbd> | Toggle follow-tail (`LIVE`/`HOLD`)
<kbd>w</kbd> | Toggle soft wrapping
<kbd>n</kbd> / <kbd>p</kbd> | Next/previous Error, Fatal, Assert, or stack frame
<kbd>c</kbd> | Clear the device log and local backlog
<kbd>D</kbd> | Rediscover and switch Android devices
<kbd>r</kbd> | Reconnect the selected device immediately
<kbd>V</kbd> | Select Raw, Compact, Process, or Full display fields
<kbd>h</kbd> | Select a recent query
<kbd>N</kbd> | Apply a named saved query
<kbd>C-c C-s</kbd> | Save the current query by name
<kbd>o</kbd> | Run `occur`
<kbd>s</kbd> | Save the buffer and stop Logcat
<kbd>g</kbd> | Show detailed stream and filter status
<kbd>M-c</kbd> | Toggle case-sensitive query matching
<kbd>q</kbd> | Stop Logcat and close the buffer

All primary commands are available both directly and from the Transient menu.

The stream collects Android's `main`, `system`, `radio`, `events`, `crash`, and
`kernel` ring buffers by default so diagnostic messages remain available to
structured queries.

## Configuration

```elisp
(setq elogcat-backlog-size (* 8 1024 1024) ; retained characters
      elogcat-soft-wrap t                   ; default for new buffers
      elogcat-default-tail 100              ; initial device history
      elogcat-default-query "package:mine"   ; Android Studio default
      elogcat-default-device-serial nil       ; discover/prompt for device
      elogcat-auto-reconnect-attempts 5       ; bounded reconnect retries
      elogcat-package-cache-ttl 60            ; cache package UID metadata
      elogcat-default-visible-fields '(raw)   ; or structured field list
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
