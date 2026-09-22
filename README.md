# elogcat.el [![MELPA](http://melpa.org/packages/elogcat-badge.svg)](http://melpa.org/#/elogcat)

logcat interface for Emacs based on [android-mode](https://github.com/remvee/android-mode).

`elogcat` keeps a bounded, structured backlog of `adb logcat -v threadtime`
records. Filters and minimum levels are applied to the retained backlog
immediately, without restarting adb. Pausing only stops rendering: incoming
messages remain available when the stream resumes. The mode line reports
`LIVE`, `HOLD`, or `PAUSED`, plus `WRAP` when soft wrapping is enabled.

Multi-line exceptions retain their header metadata, so tag, PID, and level
filters keep stack traces together. Error/Fatal/Assert headers and stack frames
can be browsed as one wrapping occurrence sequence.

Package filtering follows Android Studio's client-side model. `P` selects an
installed package without restarting adb or clearing the backlog. `elogcat`
periodically associates package UIDs with running PIDs and process names, so
filters follow app restarts and include remote processes such as
`com.example.app:worker`. Error, Fatal, and Assert messages emitted by proxy
processes such as `AndroidRuntime` are retained when their complete message
mentions the selected package.

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
<kbd>SPC</kbd> | Pause/resume rendering (messages continue entering the backlog)
<kbd>f</kbd> | Toggle follow-tail (`LIVE`/`HOLD`)
<kbd>W</kbd> | Toggle soft wrapping
<kbd>n</kbd> / <kbd>p</kbd> | Next/previous Error, Fatal, Assert, or stack frame
<kbd>C</kbd> | Clear the device log and local backlog
<kbd>/</kbd> | Set or clear an Android Studio compatible filter expression
<kbd>M-c</kbd> | Toggle case-sensitive structured query matching
<kbd>i</kbd> / <kbd>x</kbd> | Set include/exclude regexp and redraw immediately
<kbd>I</kbd> / <kbd>X</kbd> | Clear include/exclude regexp
<kbd>L</kbd> | Set minimum log level and redraw immediately
<kbd>P</kbd> | Toggle local structured filtering by an installed package
<kbd>g</kbd> | Show stream and filter status
<kbd>F</kbd> | Run `occur`
<kbd>S</kbd> | Save the buffer and stop Logcat
<kbd>q</kbd> | Stop Logcat and close the buffer
<kbd>m</kbd> | Toggle the `main` ring buffer
<kbd>s</kbd> | Toggle the `system` ring buffer
<kbd>r</kbd> | Toggle the `radio` ring buffer
<kbd>e</kbd> | Toggle the `events` ring buffer
<kbd>c</kbd> | Toggle the `crash` ring buffer
<kbd>k</kbd> | Toggle the `kernel` ring buffer

## Configuration

```elisp
(setq elogcat-backlog-size (* 8 1024 1024) ; retained characters
      elogcat-soft-wrap t                   ; default for new buffers
      elogcat-default-tail 100              ; initial device history
      elogcat-process-refresh-interval 2)    ; package/PID refresh seconds
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
