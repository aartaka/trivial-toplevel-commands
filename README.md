<h1>Trivial Toplevel Commands</h1>

<p>
Most CL implementations provide convenient toplevel commands, like

<pre lang=lisp>;; ECL command to load a Lisp file
:ld file.lisp
</pre>

<p>
These commands tend to start with a colon and end with a newline,
possibly including space-separated arguments. Such commands are too
similar to not want a portable way to define them. Trivial Toplevel
Commands (TPL-CMDS) is such a portability layer. It allows to define and remove
toplevel commands on SBCL, CCL, ECL, ABCL, CLISP, and Allegro CL.

<p>
Couple of caveats:

<DL><dt> THERE ARE NO TOPLEVEL COMMANDS ON SBCL </dt> <dd>
there are only debugger
commands. Trivial Toplevel Commands only defines debugger command to
not interfere with default SBCL REPL. If you want to access defined
commands, enter the debugger by signaling some condition (<code>(break)</code>,
at least) or contribute a more involved REPL to SBCL directly.
 <UL><li> You can also ~(require "sb-aclrepl")~ to enable an Allegro-like
    REPL. Trivial Toplevel Commands supports it.
 </UL>
</dd><dt> CCL COMMANDS ONLY ACCEPT EVALUATED ARGUMENTS </dt> <dd>
macro-like commands
are thus not possible. If you want to have maximum portability, only
use <code>define-toplevel-command/eval</code> or constant arguments. Contribute
a new REPL to CCL if you want to change that.
</dd><dt> HELP WITH MAKING IT WORK ON CLASP, CMUCL, AND LISPWORKS </dt> <dd>
I don't
have access to these implementations. Like, at all. Thanks.
</DL>

</SECTION> <SECTION id=getting-started><h2><a href=#getting-started>Getting Started</a></h2>

<p>
Clone the Git repository:
<pre>git clone --recursive https://github.com/aartaka/trivial-toplevel-commands ~/common-lisp/
</pre>

<p>
And then load <code>:trivial-toplevel-commands</code> in the REPL:

<pre lang=lisp>(asdf:load-system :trivial-toplevel-commands)
;; or, if you use Quicklisp
(ql:quickload :trivial-toplevel-commands)
</pre>

You can also install TPL-CMDS via Guix, using the
bundled <code>guix.scm</code> file:

<pre>guix package -f guix.scm
</pre>

</SECTION> <SECTION id=examples><h2><a href=#examples>Examples</a></h2>

<p>
A simple shell-invoking command mimicking <a href="https://github.com/ruricolist/cmd/">ruricolist/cmd</a>.
Obviously quite primitive:

<pre lang=lisp>(tpl-cmds:define-command/eval (:cmd :!) (command)
  "Shell invocation shortcut."
  (uiop:launch-program command))

;; Used like:
:! "touch ~/cmd-test.txt"

;; With an apparent result
(uiop:file-exists-p &num;p"~/cmd-test.txt")
;; => &num;P"~/cmd-test.txt"
</pre>

<p>
A much more involved command processing raw string argument,
say, for your MP3 player setup:

<pre lang=lisp>(tpl-cmds:define-command/string (:list-mp3s :lm) (path)
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
</pre>

<p>
You can also undefine previously defined commands with <code>remove-command</code>:

<pre lang=lisp>;; By alias:
(tpl-cmds:remove-command :lm)
;; Or by full name:
(tpl-cmds:remove-command :list-mp3s)
</pre>

<p>
Most of the information for command is accessible too, via helper
functions:

<DL><dt> Command char for implementation (setf-able) </dt> <dd>
<code>command-char</code>.
</dd><dt> Command alias by name </dt> <dd> <code>command-alias</code>.
</dd><dt> And vice versa </dt> <dd> <code>command-name</code>.
</dd><dt> The handler function for the command </dt> <dd> <code>command-handler</code>.
 <UL><li> Documentation for the command can be fetched with <code>(documentation (command-handler :command) 'function)</code>.
 </UL>
</DL>

<p>
You can also see the practical uses for commands
<a href="https://github.com/aartaka/lisp-config/blob/master/config.lisp">in my config</a>.

</SECTION> <SECTION id=similar-libraries><h2><a href=#similar-libraries>Similar Libraries</a></h2>

<DL><dt> <a href="https://web.archive.org/web/20160826073800/http://heim.ifi.uio.no/~pok/download/commands.lisp">commands.lisp by Peder Klingenberg</a> </dt> <dd>
CMUCL-specific, could possibly work on SBCL.
</dd><dt> <a href="https://web.archive.org/web/20170511215618/http://users.actrix.co.nz/mycroft/toplevel.tar.gz">Toplevel by Paul Foley</a> </dt> <dd>
CMUCL-specific.
</DL>
