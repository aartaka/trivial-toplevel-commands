;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(defsystem "trivial-toplevel-commands"
  :description "Trivial Toplevel Commands allows to define toplevel commands available on most implementations in a portable fashion."
  :author "Artyom Bologov"
  :homepage "https://github.com/aartaka/trivial-toplevel-commands"
  :bug-tracker "https://github.com/aartaka/trivial-toplevel-commands/issues"
  :source-control (:git "https://github.com/aartaka/trivial-toplevel-commands.git")
  :license  "BSD-3 Clause"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "commands")))
