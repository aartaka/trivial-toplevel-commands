#include "preprocessor.h"
#define PRE() <pre>
#define PRECAP() </pre>
#define P() <p>
<h1>Trivial Toplevel Commands</h1>

P()
Most CL implementations provide convenient toplevel commands, like

PRE();; ECL command to load a Lisp file
:ld file.lisp
PRECAP()

P()
These commands tend to start with a colon and end with a newline,
possibly including space-separated arguments. Such commands are too
similar to not want a portable way to define them. Trivial Toplevel
Commands (TPL-CMDS) is such a portability layer. It allows to define and remove
toplevel commands on SBCL, CCL, ECL, ABCL, CLISP, and Allegro CL.

P()
Couple of caveats:

DL(THERE ARE NO TOPLEVEL COMMANDS ON SBCL)
there are only debugger
commands. Trivial Toplevel Commands only defines debugger command to
not interfere with default SBCL REPL. If you want to access defined
commands, enter the debugger by signaling some condition (CD((break)),
at least) or contribute a more involved REPL to SBCL directly.
 ULI You can also ~(require "sb-aclrepl")~ to enable an Allegro-like
    REPL. Trivial Toplevel Commands supports it.
 END(UL)
DD(CCL COMMANDS ONLY ACCEPT EVALUATED ARGUMENTS)
macro-like commands
are thus not possible. If you want to have maximum portability, only
use CD(define-toplevel-command/eval) or constant arguments. Contribute
a new REPL to CCL if you want to change that.
DD(HELP WITH MAKING IT WORK ON CLASP, CMUCL, AND LISPWORKS)
I don't
have access to these implementations. Like, at all. Thanks.
END(DL)

SECTION2(getting-started, Getting Started)

P()
Clone the Git repository:
PRE()git clone --recursive https://github.com/aartaka/trivial-toplevel-commands ~/common-lisp/
PRECAP()

P()
And then load CD(:trivial-toplevel-commands) in the REPL:

PRE()(asdf:load-system :trivial-toplevel-commands)
;; or, if you use Quicklisp
(ql:quickload :trivial-toplevel-commands)
PRECAP()

You can also install TPL-CMDS via Guix, using the
bundled CD(guix.scm) file:

PRE()guix package -f guix.scm
PRECAP()

SECTION2(examples, Examples)

P()
A simple shell-invoking command mimicking A("https://github.com/ruricolist/cmd/", ruricolist/cmd).
Obviously quite primitive:

PRE()(tpl-cmds:define-command/eval (:cmd :!) (command)
  "Shell invocation shortcut."
  (uiop:launch-program command))

;; Used like:
:! "touch ~/cmd-test.txt"

;; With an apparent result
(uiop:file-exists-p &num;p"~/cmd-test.txt")
;; => &num;P"~/cmd-test.txt"
PRECAP()

P()
A much more involved command processing raw string argument,
say, for your MP3 player setup:

PRE()(tpl-cmds:define-command/string (:list-mp3s :lm) (path)
  "Recursively list all MP3 files in PATH, be it directory or file."
  (labels ((ls (file)
             (cond
               ((uiop:directory-pathname-p file)
                (format t "~&Directory ~a..." file)
                (mapc #'ls (uiop:subdirectories file))
                (mapc #'ls (uiop:directory-files file)))
               ((equalp "mp3" (pathname-type file))
                (format t "~&File ~a..." file)))))
    (ls (uiop:merge-pathnames* (uiop:parse-native-namestring path) (uiop:getcwd)))))

;; Used like:
:list-mp3s /path/to/your/mp3/files
;; or
:lm /path/to/your/mp3/files

;; Directory ...
;; Directory ...
;; File ...
;; Directory ...
;; Directory ...
;; File ...
;; File ...
;; File ...
;; File ...
PRECAP()

P()
You can also undefine previously defined commands with CD(remove-command):

PRE();; By alias:
(tpl-cmds:remove-command :lm)
;; Or by full name:
(tpl-cmds:remove-command :list-mp3s)
PRECAP()

P()
Most of the information for command is accessible too, via helper
functions:

DL(Command char for implementation (setf-able))
CD(command-char).
DD(Command alias by name) CD(command-alias).
DD(And vice versa) CD(command-name).
DD(The handler function for the command) CD(command-handler).
 ULI Documentation for the command can be fetched with <code>(documentation (command-handler :command) 'function)</code>.
 END(UL)
END(DL)

P()
You can also see the practical uses for commands
A("https://github.com/aartaka/lisp-config/blob/master/config.lisp", in my config).

SECTION2(similar-libraries, Similar Libraries)

DL(A("https://web.archive.org/web/20160826073800/http://heim.ifi.uio.no/~pok/download/commands.lisp", commands.lisp by Peder Klingenberg))
CMUCL-specific, could possibly work on SBCL.
DD(A("https://web.archive.org/web/20170511215618/http://users.actrix.co.nz/mycroft/toplevel.tar.gz", Toplevel by Paul Foley))
CMUCL-specific.
END(DL)