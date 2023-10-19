;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

(uiop:define-package :trivial-toplevel-commands/commands
  (:documentation "Package to INTERN command names to."))

(uiop:define-package :trivial-toplevel-commands
  (:nicknames :toplevel-commands :tpl-cmds)
  (:use :common-lisp)
  (:export #:define-command/string #:define-command/read #:define-command/eval
           #:remove-command
           #:command-alias #:command-name)
  (:documentation "`trivial-toplevel-commands' allows (un)defining new toplevel commands.

There are three macros defining new commands:

- `define-command/string' to define commands that process a single
  string argument—the rest of the command invocation.
 - `define-command/read' macro binds a new command that processed the
   arguments passed to it without evaluating them.
 - And `define-command/eval' doing the same but with arguments
   evaluated.

`remove-command' unbinds the defined command. By full name or alias.

`command-alias', `command-name', and `command-handler' allow to get
the name, alias, and handler of command by name/alias.

See function/macro docstrings and README file for usage examples.

TODO: Examples should belong to macros/functions/package themselves,
not to the README..."))

(in-package :trivial-toplevel-commands)

(defun string-slurp-forms (string)
  (unless (uiop:emptyp string)
    (with-input-from-string (s string)
      (uiop:slurp-stream-forms s))))

#+clisp
(defvar name->fn (make-hash-table :test #'equalp)
  "Table from command name to function to track `custom:*user-commands*'.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar alias->name (make-hash-table)
    "Table to find command name by alias.
Useful for `remove-command'.")
  (defvar name->alias (make-hash-table)
    "Table to find command alias by name.
Useful for `remove-command'.")
  (defvar name->handler (make-hash-table)
    "Table to find actual command handler by name.
Useful for `command-handler'."))

(defun toplevel-name (name)
  (intern (string
           (gensym
            (uiop:strcat "TPL-" name "-COMMAND")))
          :trivial-toplevel-commands/commands))

(defmacro define-command/string (name (&optional (argument (gensym "ARG"))
                                         (actual-arglist (list argument)))
                                 &body (documentation . body))
  "Define NAME command running BODY with string command ARGUMENT.

ARGUMENT is always a string, even if empty one.
- For command \":foo abc\", the ARGUMENT is \"abc\".
- For command \":foo\", the ARGUMENT is \"\".

NAME is either:
- A single symbol---full name of the new command.
- A (FULL-NAME ALIAS) list---for the aliased command.

In case NAME matches an existing command, redefine it.

DOCUMENTATION docstring should have a one-line executive summary as
the first line. That's what most implementations will show alongside
the command name/alias. The rest of the DOCUMENTATION is additional
details, which might or might not get attached to the generated
command handler, but aren't guaranteed to be anywhere, especially
across implementations.

SBCL quirk: new command is only accessible in break/debug loop."
  (declare (ignorable name argument actual-arglist documentation body))
  (check-type documentation string)
  (let* ((names (mapcar (lambda (n) (intern (string n) :keyword))
                        (uiop:ensure-list name)))
         (alias (second names))
         (name (first names))
         (toplevel-fn-name (toplevel-name (string (first names))))
         (documentation-1 (first (uiop:split-string documentation)))
         (fn-var (gensym "FN")))
    (declare (ignorable names toplevel-fn-name documentation-1 fn-var))
    (check-type names (cons keyword (or null (cons keyword null))))
    (setf (gethash alias alias->name) name)
    (setf (gethash name name->alias) alias)
    `(progn
       #-ecl
       (defun ,toplevel-fn-name (,argument)
         ,documentation
         (declare (ignorable ,argument))
         (let ((,argument (or ,argument "")))
           (declare (ignorable ,argument))
           ,@body))
       #+sbcl
       (let ((,fn-var (lambda ()
                        (funcall
                         (quote ,toplevel-fn-name)
                         (if (sb-impl::listen-skip-whitespace *debug-io*)
                             (read-line *debug-io* nil nil)
                             "")))))
         (push (cons ,(symbol-name name) ,fn-var) sb-debug::*debug-commands*)
         ,@(when alias
             `((push (cons ,(symbol-name alias) ,fn-var) sb-debug::*debug-commands*))))
       #+sbcl
       (warn "Can only define debugger command. Enter debugger to use ~s~@[/~s~]" ,name ,alias)
       #+clozure
       (warn "Cannot define string commands on CCL—only eval commands are available.")
       #+ecl
       (defun ,toplevel-fn-name (&rest ,argument)
         ,documentation
         (declare (ignorable ,argument))
         ;; FIXME: This replaces all the whitespace with a single
         ;; space. Any way to preserve it?
         (let ((,argument (format nil "~{~a~^ ~}" ,argument)))
           (declare (ignorable ,argument))
           ,@body))
       #+ecl
       (push
        (quote ((,@(when alias (list alias))
                 ,name)
                ,toplevel-fn-name :string
                ,(format nil "~(~s~)~@[/~(~s~)~] ~a" name alias documentation-1)
                ,(format nil "~(~s~) ~(~a~) [Top level command]~:[~2*~;~@
~(~s~) ~(~a~) [Abbreviation]~@
~]
~@
~a"
                         name actual-arglist
                         alias alias actual-arglist
                         documentation)))
        (rest (find "Top level commands" system::*tpl-commands*
                    :key #'first
                    :test #'string-equal)))
       #+abcl
       ,(let ((lowercase-name (format nil "~(~a~)" name)))
          `(push (quote
                  (,lowercase-name ,(when alias
                                      (format nil "~(~a~)" alias))
                                   ,toplevel-fn-name ,documentation-1))
                 tpl::*command-table*))
       #+clisp
       (when (uiop:emptyp custom:*user-commands*)
         (push (lambda () (list (format nil "~2%User-defined commands:")))
               custom:*user-commands*))
       #+clisp
       ,@(let ((lowercase-name (format nil "~(~s~)" name))
               (capitalized-name (format nil "~:(~a~)" name))
               (lowercase-alias (format nil "~(~s~)" alias))
               (fn (gensym "FN")))
           `((setf custom:*user-commands*
                   (let ((,fn (lambda ()
                                (append
                                 (list (format nil "~%~a/~a~@[/~a~] ~{~a~^ ~} = ~a"
                                               ,lowercase-name ,capitalized-name ,lowercase-alias
                                               (quote ,actual-arglist) ,documentation-1)
                                       (cons ,(format nil "~:(~a~)" name) (function ,toplevel-fn-name))
                                       (cons ,lowercase-name (function ,toplevel-fn-name))
                                       (cons ,capitalized-name (function ,toplevel-fn-name))
                                       ,@(when alias
                                           `((cons ,lowercase-alias (function ,toplevel-fn-name)))))))))
                     (setf (gethash ,lowercase-name name->fn) ,fn)
                     (append custom:*user-commands*
                             (list ,fn))))))
       #+allegro
       (dolist (n (list ,name ,alias))
         (tpl::add-new-command
          (format nil "~(~a~)" n)
          (1- (length (string n)))
          (lambda (&optional ,argument)
            ,documentation
            (declare (ignorable ,argument))
            (funcall (quote ,toplevel-fn-name) ,argument))
          ,documentation-1
          :arg-mode :string))
       #-clozure
       (prog1
           (quote ,(first names))
         (setf (gethash (quote ,(first names)) name->handler)
               (quote ,toplevel-fn-name)))
       #+clozure nil)))

(defmacro define-command/read (name (&rest arguments) &body (documentation . body))
  "Define NAME command running BODY with raw `read' s-exprs as ARGUMENTS.

For more info, see `define-command/string'."
  (declare (ignorable name arguments documentation body))
  (let* ((names (uiop:ensure-list name))
         (toplevel-fn-name (toplevel-name (string (first names)))))
    (declare (ignorable names toplevel-fn-name))
    `(progn
       #+clozure
       (warn "Cannot define read commands on CCL—only eval commands are available.")
       #-clozure
       (defun ,toplevel-fn-name (,@arguments)
         ,documentation
         ,@body)
       #+allegro
       (dolist (n (quote ,names))
         (tpl::add-new-command
          (format nil "~(~a~)" n)
          (1- (length (string n)))
          (function ,toplevel-fn-name)
          ,(first (uiop:split-string documentation))
          :arg-mode nil))
       #-(or clozure allegro)
       (define-command/string ,name (arg)
         ,documentation
         (declare (ignorable arg))
         (apply (quote ,toplevel-fn-name)
                ,(when arguments
                   `(string-slurp-forms arg))))
       #-clozure
       (prog1
           (quote ,(first names))
         (setf (gethash (quote ,(first names)) name->handler)
               (quote ,toplevel-fn-name)))
       #+clozure nil)))

(defmacro define-command/eval (name arguments &body (documentation . body))
  "Define NAME command running BODY with `eval'-uated ARGUMENTS.

For more info, see `define-command/string'."
  (declare (ignorable name arguments documentation body))
  (let* ((arg-var (gensym "ARGS"))
         (names (uiop:ensure-list name))
         (toplevel-fn-name (toplevel-name (string (first names)))))
    (declare (ignorable arg-var names))
    #+clozure
    (when (second names)
      (setf (gethash (second names) alias->name) (first names)
            (gethash (first names) name->alias) (second names)))
    `(progn
       (defun ,toplevel-fn-name (,@arguments)
         ,documentation
         ,@body)
       #+clozure
       (let ((global-commands (assoc :global ccl::*defined-toplevel-commands*)))
         ,@(loop for name in names
                 collect `(rplacd global-commands
                                  (remove (quote ,name) (rest global-commands)
                                          :key #'first))
                 collect `(push (cons (quote ,name) (list* (function ,toplevel-fn-name)
                                                           ,documentation
                                                           (quote ,(mapcar #'symbol-name arguments))))
                                (cdr global-commands))))
       #-clozure
       (define-command/read ,name (&rest ,arg-var)
         ,documentation
         (apply (quote ,toplevel-fn-name)
                ,(when arguments
                   `(mapcar #'eval ,arg-var))))
       (prog1
           (quote ,(first names))
         (setf (gethash (quote ,(first names)) name->handler)
               (quote ,toplevel-fn-name))))))

(defun command-alias (name-or-alias)
  "Get the alias for NAME-OR-ALIASed command."
  (gethash name-or-alias name->alias
           (when (nth-value 1 (gethash name-or-alias alias->name))
             name-or-alias)))

(defun command-name (name-or-alias)
  "Get the name for NAME-OR-ALIASed command."
  (gethash name-or-alias alias->name
           (when (nth-value 1 (gethash name-or-alias name->alias))
             name-or-alias)))

(defun command-handler (name-or-alias)
  (let ((name (command-name name-or-alias)))
    (gethash name name->handler)))

(defun remove-command (name-or-alias)
  "Remove a previously defined toplevel command by NAME-OR-ALIAS.
Can also remove built-in toplevel command (except when on CLISP.)"
  (let* ((alias (command-alias name-or-alias))
         (name (command-name name-or-alias)))
    (remhash alias alias->name)
    (remhash name name->alias)
    #+sbcl
    (setf sb-debug::*debug-commands*
          (remove-if
           (lambda (cmd)
             (or (string-equal (car cmd) name)
                 (string-equal (car cmd) alias)))
           sb-debug::*debug-commands*))
    #+clozure
    (let ((global-commands (assoc :global ccl::*defined-toplevel-commands*)))
      (rplacd global-commands
              (remove alias (remove name (rest global-commands) :key #'first)
                      :key #'first)))
    #+ecl
    (loop for name+commands in system::*tpl-commands*
          for (context-name . commands) = name+commands
          when (equal context-name "Top level commands")
            do (setf (rest name+commands)
                     (remove-if (lambda (command)
                                  (or (find name (first command))
                                      (find alias (first command))))
                                commands)))
    #+abcl
    (setf tpl::*command-table*
          (remove-if (lambda (command-spec)
                       (find (format nil "~(~a~)" name)
                             command-spec
                             :test #'string-equal))
                     tpl::*command-table*))
    #+clisp
    (setf custom:*user-commands*
          (remove (gethash (format nil "~(~s~)" name) name->fn)
                  custom:*user-commands*))
    #+clisp
    (when (= 1 (length custom:*user-commands*))
      (setf custom:*user-commands* nil))
    #+allegro
    (progn
      (remhash (format nil "~(~a~)" name) tpl::*command-hash-table*)
      (remhash (format nil "~(~a~)" alias) tpl::*command-hash-table*))
    (values)))

