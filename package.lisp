;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package :trivial-toplevel-commands/commands
  (:documentation "Package to INTERN command names to."))

(uiop:define-package :trivial-toplevel-commands
  (:nicknames :toplevel-commands :tpl-cmds :tpl-cmd)
  (:use :common-lisp)
  (:export #:define-command/string #:define-command/read #:define-command/eval #:define-command
           #:defcommand/string #:defcommand/read #:defcommand/eval #:defcommand
           #:remove-command
           #:command-alias #:command-name #:command-handler
           #:command-char)
  (:documentation "`trivial-toplevel-commands' allows (un)defining new toplevel commands.

There are three macros defining new commands:

- `define-command/string' to define commands that process a single
  string argument—the rest of the command invocation.
 - `define-command/read' macro binds a new command that processed the
   arguments passed to it without evaluating them.
 - And `define-command/eval' doing the same but with arguments
   evaluated.
- Shorter `defcommand/*' versions are available too.

`remove-command' unbinds the defined command. By full name or alias.

`command-alias', `command-name', and `command-handler' allow to get
the name, alias, and handler of command by name/alias.

See function/macro docstrings and README file for usage examples.

TODO: Examples should belong to macros/functions/package themselves,
not to the README..."))

#+sbcl
(when (find "SB-ACLREPL" *modules* :test #'string=)
  (pushnew :sb-aclrepl *features*))
