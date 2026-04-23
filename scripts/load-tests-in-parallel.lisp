;;; Run as: scripts/z-load-in-parallel

;; create the output string first and then print it, to reduce message
;; interference.
(defmacro err-out (fmt &rest params)
  `(princ (format nil ,fmt ,@params) *error-output*))
(defmacro string+ (&rest strings)
  `(concatenate 'string ,@strings))


#+nil(pushnew :with-debugging *features*)
(pushnew :verbose-par-loading *features*)
;; exclude coalton-doc (& serapeum)
(pushnew :coalton-without-doc *features*)
#+unpatched-lparallel
(err-out "Running with unpatched lparallel~%")
#-unpatched-lparallel
(err-out "Running with patched lparallel~%")

(require 'asdf)

;;; https://lispcookbook.github.io/cl-cookbook/process.html

(cl:handler-case
    (require 'lparallel)
  (cl:error (c)
    (declare (ignore c))
    (asdf:load-system :lparallel)
))

;;; Patch lparallel:make-ptree, so it accepts a :test key, to allow us
;;; to use system names (strings) as ptree identifiers.
(in-package :lparallel)
(defconstant +original-make-ptree+ (symbol-function 'lparallel:make-ptree))
(defun lparallel:make-ptree ( &key (test #'eql) )
  "Create a ptree instance."
  (let ((p (funcall +original-make-ptree+)))
    (setf (slot-value p 'lparallel.ptree::nodes) (make-hash-table :test test))
    p))
(in-package :cl-user)

;; Copied from compat/compatibility-layer.lisp
(defmacro unset-all-float-traps ()
  '(cl:eval-when (:compile-toplevel :load-toplevel :execute)
    #+ccl (ccl:set-fpu-mode :overflow nil :underflow nil :division-by-zero nil :invalid nil :inexact nil)
    #+sbcl (sb-int:set-floating-point-modes :traps nil)
    #+abcl (extensions:set-floating-point-modes :traps nil)
    #+ecl  (ext:trap-fpe 'cl:t nil)
    #+clasp (core:fe-disable-except (logior core:+fe-underflow+
                                            core:+fe-overflow+
                                            core:+fe-invalid+
                                            core:+fe-inexact+
                                            core:+fe-divbyzero+))
    #-(or sbcl allegro ccl abcl ecl clasp)
    #.(cl:error "don't know how to unset all float traps on ~A" (cl:lisp-implementation-type))
    ))

;; This is from coalton's scripts/dependency-audit.lisp
(defun form-head-name (form)
  (and (consp form)
       (symbolp (first form))
       (string-upcase (symbol-name (first form)))))

(defun dep-in-treep (dep dep-tree)
  (find dep dep-tree :key #'car :test #'equalp))

(defun get-system-deps (system)
  (labels ((get-system-deps-helper (system dep-tree)
             (let ((deps (asdf:system-depends-on (asdf:find-system system))))
               #+with-debugging(err-out "~%The deps of ~A are: ~A "
                                        system deps)
               (if (null deps)
                   (when (not (dep-in-treep system dep-tree))
                     (push (list system) dep-tree))
                   (progn
                     (dolist (dep deps)
                       (cond
                         ((stringp dep)
                          (when (not (dep-in-treep dep dep-tree))
                            (setq dep-tree
                                  (get-system-deps-helper dep dep-tree))))
                         ((symbolp dep)
                          (when (not (dep-in-treep dep dep-tree))
                            (setq dep-tree
                                  (get-system-deps-helper (symbol-name dep)
                                                          dep-tree))))
                         ;; Treatment of next two cases
                         ;; (version/feature) - code adapted from
                         ;; coalton's scripts/dependency-audit.lisp
                         ((equal (form-head-name dep) "VERSION")
                          (let ((the-dep (second dep)))
                            (when (not (dep-in-treep the-dep dep-tree))
                              (setq dep-tree
                                    (get-system-deps-helper the-dep
                                                            dep-tree)))))
                         ((equal (form-head-name dep) "FEATURE")
                          #+with-debugging
                          (err-out "Read a feature ~A " dep)
                          (destructuring-bind
                              (_ feature dependency &rest ignored)
                              dep
                            ;; (declare (ignore _ ignored))
                            (when (uiop:featurep feature)
                              (if (atom dependency)
                                  (when (not (dep-in-treep dependency dep-tree))
                                    (setq dep-tree
                                          (get-system-deps-helper dependency
                                                                  dep-tree)))
                                  (when (not (equal (form-head-name dependency)
                                                    "REQUIRE"))
                                    ;; (setq dep-tree
                                    ;;    (get-system-deps-helper
                                    ;;     (second dependency)
                                    ;;     dep-tree))
                                    (err-out
                                     (string+ "~%ERROR feature asks for a cons"
                                              " that's not a REQUIRE:"
                                              "~%system ~A dep ~A _ ~A"
                                              " feature ~A"
                                              " dependency ~A ignored ~A~%")
                                     system dep _ feature dependency ignored)
                                    (break))))))
                         (t
                          (progn
                            (err-out (string+ "~%Error - hit an unknown ASDF"
                                              " cons dependency case:"
                                              "~%system ~A dep ~A~%")
                                     system dep)
                            (break)))))
                     (when (not (dep-in-treep system dep-tree))
                       (push (cons system
                                   (remove-if-not #'(lambda (dep)
                                                      (or (stringp dep)
                                                          (symbolp dep)))
                                                  deps))
                             dep-tree)))))))
    (let* ((all-deps (get-system-deps-helper system nil))
           (sorted-system-deps
             (sort (copy-tree all-deps)
                   #+nil(remove-duplicates all-deps :test #'equalp)
                   #'(lambda (x y) (< (length x) (length y)))))
           (orphan-system-deps
             (set-difference
              (apply #'append (mapcar #'rest sorted-system-deps))
              (mapcar #'first sorted-system-deps)
              :test #'equalp)))
      #(or +with-debugging verbose-par-loading)
      (err-out "all-deps : ~%~A~%sorted-deps :~%~A~%orphan-system-deps :~%~A~%"
               all-deps
               sorted-system-deps
               orphan-system-deps)
      (list sorted-system-deps orphan-system-deps))))

(defun ptree-id-from-string (str)
  #+unpatched-lparallel
  (intern str "temp")
  #-unpatched-lparallel
  str)

(defun strings-to-ptree-ids (tree)
  "Replaces strings in the tree with a ptree id object"
  #+unpatched-lparallel
  ;; No longer necessary - patched lparallel:make-ptree
  (progn
    (when (not (find-package "temp"))
      (make-package "temp"))
    (mapcar #'(lambda (lst)
                (mapcar #'(lambda (system)
                            (ptree-id-from-string system))
                        lst))
            tree))
  #-unpatched-lparallel
  tree)

(defmacro load-system-in-parallel (system)
  `(let* ((all-deps (get-system-deps ,system))
          (deps (strings-to-ptree-ids (first all-deps)))
          (orphan-deps (strings-to-ptree-ids (rest all-deps)))
          (target (gensym "target"))
          ;; https://sharplispers.github.io/lparallel/Ptrees.html
          (tree
            #+unpatched-lparallel
            (lparallel:make-ptree)
            #-unpatched-lparallel
            (lparallel:make-ptree :test #'equalp))
          (ptrees
            (map 'list
                 #'(lambda (lst)
                     (let ((to-load (first lst))
                           (has-dependencies (rest lst)))
                       (lparallel:ptree-fn
                        to-load
                        has-dependencies
                        (lambda (&rest dependencies)
                          (let ((dep-lngth (length dependencies)))
                            #+(or with-debugging verbose-par-loading)
                            (err-out (string+ "Starting to load system ~A,"
                                              " after its dependencies ~A (~A)"
                                              " have been loaded~%")
                                     to-load has-dependencies dependencies)
                            (cl:handler-case
                                (cl:progn
                                  (asdf:load-system to-load)
                                  #+with-debugging
                                  (err-out "Loaded ~A~%" to-load)
                                  dep-lngth)
                              (cl:error (c)
                                (declare (ignore c))
                                (err-out "Failed to load ~A~%" to-load)
                                (if (= 0 dep-lngth) -0.1 (- dep-lngth))))))
                        tree)))
                 deps))
          (orphan-ptrees
            (mapcar #'(lambda (dep)
                        (lparallel:ptree-fn
                         dep
                         '()
                         (lambda ()
                           (err-out "Ignoring ~A~%" dep)
                           ;; return a `success' value for consistency
                           ;; with other ptrees
                           0)
                         tree))
                    orphan-deps)))
     #+with-debugging(err-out " ptrees :~%~A~% orphan-ptrees : ~%~A~%"
                              ptrees orphan-ptrees)
     #-with-debugging(declare (ignore ptrees orphan-ptrees))
     ;;; The ptrees have already been evaluated, no need to "evaluate"
     ;;; them further.
     ;; (eval (cons 'progn (append ptrees orphan-ptrees)))
     (lparallel:ptree-fn target
                         (list (ptree-id-from-string ,system))
                         (lambda (pk)
                           (err-out "Finished loading ~A~%" pk)
                           ;; return a `success' value for consistency
                           ;; with other ptrees
                           0)
                         tree)
     ;; launch all ptrees!
     (lparallel:call-ptree target tree)))

;; Unused.
#+dead-code
(defconstant +delay-max+ 20)
#+dead-code
(defun load-with-random-delay (system &key (load #'asdf:load-system))
  ;; introduce a random delay, to reduce potential contention on any
  ;; system resources while compiling/saving code.
  (sleep (random +delay-max+))
  (funcall load system))

;; ``20'' - on Linux: nproc --all - see serapeum/threads.lisp
(setf lparallel:*kernel* (lparallel:make-kernel 20 :name "custom-kernel"))

(defun do-load-subsystems ()
  ;; load all package dependencies
  (err-out "XXX-*Parallel* loading coalton package dependencies~%")
  (time (load-system-in-parallel "coalton"))
  (err-out "XXX-*Parallel* loading coalton package dependencies~%")
  (time (load-system-in-parallel "coalton-asdf"))
  ;; These two cannot be loaded in parallel for some unknown reason -
  ;; using plain ASDF.
  (err-out "XXX-*ASDF* loading quil-coalton/tests package dependencies~%")
  (time (asdf:load-system "quil-coalton/tests"))
  (err-out "XXX-*ASDF* loading thih-coalton/tests package dependencies~%")
  (time (asdf:load-system "thih-coalton/tests"))
  ;;
  ;; The only thing that this function should be doing really if not for
  ;; quil-coalton/tests & thih-coalton/tests
  ;;
  (err-out "XXX-*Parallel* loading coalton/tests package dependencies~%")
  (time (load-system-in-parallel "coalton/tests")))

(defun do-run-tests ()
  (do-load-subsystems)
  ;; now run the tests
  (unset-all-float-traps)
  (err-out "XXX-Running coalton/tests~%")
  (time (asdf:test-system :coalton))

  (err-out "XXX-Loading small coalton programs~%")
  (time (asdf:load-system :small-coalton-programs))
  (err-out "SUCCESS!~%"))

(defun do-run-tests-quit-on-error ()
  (cl:handler-case
    (cl:progn
      (do-run-tests)
      (lparallel:end-kernel :wait t)
      (uiop:quit 0))
  (cl:error (c)
    (declare (ignore c))
    (err-out "FAILURE!~%")
    (lparallel:end-kernel :wait t)
    (uiop:quit 1))))

;; Potential tasks
#+nil (do-load-subsystems)

#+nil (do-run-tests)

#+nil (do-run-tests-quit-on-error)

