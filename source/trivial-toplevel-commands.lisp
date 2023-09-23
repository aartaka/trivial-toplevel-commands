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
  (defvar *command-arglists* (make-hash-table)
    "Additional command metadata."))

(defmacro define-command/string (name (&optional (argument (gensym "ARG"))
                                         (actual-arglist (list argument)))
                                 &body (documentation . body))
  "Define NAME command running BODY with string command ARGUMENT.
For command \":foo abc\", the ARGUMENT is bound to \"abc\" string.

NAME is either:
- A single symbol---full name of the new command.
- A (FULL-NAME ALIAS) list---for the aliased command.

In case NAME matches an existing command, redefine it.

SBCL quirk: new command is only accessible in break/debug loop."
  (declare (ignorable name argument actual-arglist documentation body))
  (check-type documentation string)
  (let* ((names (mapcar (lambda (n) (intern (string n) :keyword))
                        (uiop:ensure-list name)))
         (alias (second names))
         (name (first names))
         (toplevel-fn-name (gensym (uiop:strcat
                                    "TPL-"
                                    (string (first names)) "-COMMAND")))
         (documentation-1 (first (uiop:split-string documentation)))
         (fn-var (gensym "FN")))
    (declare (ignorable names toplevel-fn-name documentation-1 fn-var))
    (check-type names (cons keyword (or null (cons keyword null))))
    (setf (gethash alias alias->name) name)
    (setf (gethash name name->alias) alias)
    `(progn
       #+sbcl
       (let ((,fn-var (lambda ()
                        (funcall
                         (lambda (,argument)
                           (declare (ignorable ,argument))
                           ,@body)
                         (if (sb-impl::listen-skip-whitespace *debug-io*)
                             (read-line *debug-io* nil nil)
                             "")))))
         (push (cons ,(symbol-name name) ,fn-var) sb-debug::*debug-commands*)
         ,@(when alias
             `((push (cons ,(symbol-name alias) ,fn-var) sb-debug::*debug-commands*))))
       #+sbcl
       (warn "Can only define debugger command. Enter debugger to use ~a" ,name)
       #+clozure
       (warn "Cannot define string commands on CCL—only eval commands are available.")
       #+ecl
       (defun ,toplevel-fn-name (&rest ,argument)
         ,documentation
         ;; FIXME: This replaces all the whitespace with a single
         ;; space. Any way to preserve whitespace?
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
       #+(or abcl clisp)
       (defun ,toplevel-fn-name (,argument)
         ,documentation
         (declare (ignorable ,argument))
         #+clisp
         (setf ,argument (if (uiop:emptyp ,argument)
                             ""
                             (subseq ,argument 1)))
         ,@body)
       #+abcl
       ,(let ((lowercase-name (format nil "~(~a~)" name)))
          `(push '(,lowercase-name ,(when alias
                                      (format nil "~(~a~)" alias))
                   ,toplevel-fn-name ,documentation-1)
                 tpl::*command-table*))
       #+clisp
       (when (uiop:emptyp custom:*user-commands*)
         (push (lambda () (list (format nil "~2%User-defined commands:")))
               custom:*user-commands*))
       #+clisp
       ,@(let ((lowercase-name (format nil "~(~s~)" name))
               (lowercase-alias (format nil "~(~s~)" alias))
               (fn (gensym "FN")))
           `((setf custom:*user-commands*
                   (let ((,fn (lambda ()
                                (append
                                 (list (format nil "~%~a~@[/~a~] ~{~a~^ ~} = ~a"
                                               ,lowercase-name ,lowercase-alias
                                               (quote ,actual-arglist) ,documentation-1)
                                       (cons ,lowercase-name (function ,toplevel-fn-name))
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
            ,@body)
          ,documentation-1
          :arg-mode :string))
       #-clozure (quote ,(first names))
       #+clozure nil)))

(defmacro define-command/read (name (&rest arguments) &body (documentation . body))
  "Define NAME command running BODY with raw `read' s-exprs as ARGUMENTS.

For more info, see `define-command/string'."
  (declare (ignorable name arguments documentation body))
  (let ((names (uiop:ensure-list name))
        (arg-var (gensym "ARG")))
    (declare (ignorable arg-var names))
    `(progn
       #+clozure
       (warn "Cannot define read commands on CCL—only eval commands are available.")
       #-clozure
       (define-command/string ,name (,arg-var ,arguments)
         ,documentation
         (apply (lambda (,@arguments)
                  ,documentation
                  ,@body)
                ,(when arguments
                   `(string-slurp-forms ,arg-var))))
       #+clozure nil
       #-clozure (quote ,(first (uiop:ensure-list names))))))

(defmacro define-command/eval (name arguments &body (documentation . body))
  "Define NAME command running BODY with `eval'-uated ARGUMENTS.

For more info, see `define-command/string'."
  (declare (ignorable name arguments documentation body))
  (let ((arg-var (gensym "ARGS"))
        (names (uiop:ensure-list name)))
    (declare (ignorable arg-var names))
    #+clozure
    (when (second names)
      (setf (gethash (second names) alias->name) (first names)
            (gethash (first names) name->alias) (second names)))
    `(progn
       #+clozure
       ,@(loop for name in (uiop:ensure-list name)
               collect `(ccl::define-toplevel-command :global ,name (,@arguments)
                          ,documentation
                          ,@body))
       #-clozure
       (define-command/string ,name (,arg-var ,arguments)
         ,documentation
         (apply (lambda (,@arguments)
                  ,documentation
                  ,@body)
                ,(when arguments
                   `(mapcar #'eval (string-slurp-forms ,arg-var)))))
       (quote ,(first (uiop:ensure-list name))))))

(defun remove-command (name)
  "Remove a previously defined toplevel command by NAME (can be alias).
Can also remove built-in toplevel command (except when on CLISP.)"
  (let* ((alias (cond
                  ((nth-value 1 (gethash name alias->name))
                   name)
                  ((nth-value 1 (gethash name name->alias))
                   (gethash name name->alias))))
         (name (if alias
                   (gethash alias alias->name)
                   name)))
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
    (dolist (n (list name alias))
      (let ((group-commands (find :global ccl::*defined-toplevel-commands*
                                  :key #'first)))
        (setf (rest group-commands)
              (remove n (rest group-commands) :key #'first))))
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
