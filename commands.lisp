;;;; SPDX-FileCopyrightText: Artyom Bologov
;;;; SPDX-License-Identifier: BSD-3 Clause

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

SBCL quirk: new command is only accessible in break/debug loop or via
ACLREPL contrib:

(require \"sb-aclrepl\")"
  (declare (ignorable name argument actual-arglist documentation body))
  (check-type documentation string "Documentation string")
  (let* ((names (mapcar (lambda (n) (intern (string n) :keyword))
                        (uiop:ensure-list name)))
         (alias (second names))
         (name (first names))
         (toplevel-fn-name (toplevel-name (string (first names))))
         (documentation-1 (first (uiop:split-string documentation :separator '(#\Newline))))
         (fn-var (gensym "FN")))
    (declare (ignorable names toplevel-fn-name documentation-1 fn-var))
    (check-type names (cons keyword (or null (cons keyword null))))
    (setf (gethash alias alias->name) name)
    (setf (gethash name name->alias) alias)
    `(progn
       #-ecl
       (defun ,toplevel-fn-name (&optional ,argument)
         ,documentation
         (declare (ignorable ,argument))
         (let ((,argument (or ,argument "")))
           (declare (ignorable ,argument))
           ,@body
           (values)))
       #+sb-aclrepl
       (dolist (n (list ,name ,alias))
         (sb-aclrepl::add-cmd-table-entry
          (format nil "~(~a~)" n)
          (length (string n))
          (quote ,toplevel-fn-name)
          ,documentation-1
          :string))
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
       #+(and sbcl (not sb-aclrepl))
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
           ,@body
           (values)))
       #+ecl
       (push
        (quote ((,@(when alias (list alias))
                 ,name)
                ,toplevel-fn-name :string
                ,(format nil "~(~s~)~@[/~(~s~)~]~17t~a" name alias documentation-1)
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
  (check-type documentation string "Documentation string")
  (let* ((names (uiop:ensure-list name))
         (toplevel-fn-name (toplevel-name (string (first names))))
         (documentation-1 (first (uiop:split-string documentation :separator '(#\Newline))))
         (arg-var (gensym "ARG")))
    (declare (ignorable names toplevel-fn-name documentation-1 arg-var))
    `(progn
       #+clozure
       (warn "Cannot define read commands on CCL—only eval commands are available.")
       #-clozure
       (defun ,toplevel-fn-name (,@arguments)
         ,documentation
         ,@body
         (values))
       #+allegro
       (dolist (n (quote ,names))
         (tpl::add-new-command
          (format nil "~(~a~)" n)
          (1- (length (string n)))
          (function ,toplevel-fn-name)
          ,documentation-1
          :arg-mode nil))
       #-(or clozure allegro)
       (define-command/string ,name (,arg-var)
         ,documentation
         (declare (ignorable ,arg-var))
         (apply (quote ,toplevel-fn-name)
                ,(when arguments
                   `(string-slurp-forms ,arg-var))))
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
  (check-type documentation string "Documentation string")
  (let* ((arg-var (gensym "ARGS"))
         (names (uiop:ensure-list name))
         (alias (second names))
         (name (first names))
         (toplevel-fn-name (toplevel-name (string (first names)))))
    (declare (ignorable arg-var names alias))
    #+(or clozure ecl)
    (when (second names)
      (setf (gethash (second names) alias->name) (first names)
            (gethash (first names) name->alias) (second names)))
    `(progn
       (defun ,toplevel-fn-name (,@arguments)
         ,documentation
         ,@body
         (values))
       #+ecl
       (push
        (quote ((,@(when alias (list alias))
                 ,name)
                ,toplevel-fn-name :eval
                ,(format nil "~(~s~)~@[/~(~s~)~]~17t~a"
                         name alias
                         (first (uiop:split-string documentation
                                                   :separator '(#\Newline))))
                ,(format nil "~(~s~) ~(~a~) [Top level command]~:[~2*~;~@
~(~s~) ~(~a~) [Abbreviation]~@
~]
~@
~a"
                         name arguments
                         alias alias arguments
                         documentation)))
        (rest (find "Top level commands" system::*tpl-commands*
                    :key #'first
                    :test #'string-equal)))
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
       #-(or ecl clozure)
       (define-command/read ,name (&rest ,arg-var)
         ,documentation
         (declare (ignorable ,arg-var))
         (apply (quote ,toplevel-fn-name)
                ,(when arguments
                   `(mapcar #'eval ,arg-var))))
       (prog1
           (quote ,(first names))
         (setf (gethash (quote ,(first names)) name->handler)
               (quote ,toplevel-fn-name))))))

(setf (macro-function 'define-command)
      (macro-function #+clozure 'define-command/eval
                      #-clozure 'define-command/read))

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
    #+sb-aclrepl
    (progn
      (remhash (format nil "~(~a~)" name) sb-aclrepl::*cmd-table-hash*)
      (remhash (format nil "~(~a~)" alias) sb-aclrepl::*cmd-table-hash*))
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

(defgeneric command-char (&optional char)
  (:documentation "Return current command char or set it to CHAR (when provided).")
  (:method (&optional char)
    (declare (ignorable char))
    #-(or abcl allegro sb-aclrepl)
    (warn "There's no command char on this implementation")
    #+(or abcl allegro sb-aclrepl)
    (if char
        (setf #+allegro tpl:*command-char*
              #+abcl tpl::*command-char*
              #+sb-aclrepl sb-aclrepl:*command-char*
              char)
        #+allegro tpl:*command-char*
        #+abcl tpl::*command-char*
        #+sb-aclrepl sb-aclrepl:*command-char*)))

(defgeneric (setf command-char) (val &optional char)
  (:method ((val character) &optional char)
    (declare (ignorable char))
    (command-char val)))

