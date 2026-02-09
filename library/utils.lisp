(defpackage #:coalton/utils
  (:use
   #:coalton)
  (:nicknames
   #:coalton-library/utils)
  (:local-nicknames
   (#:compat #:coalton-compatibility))
  (:export
   #:defstdlib-package))

(in-package #:coalton/utils)

(cl:defmacro defstdlib-package (name cl:&rest args)
  (cl:let* ((name-string (cl:string-upcase (cl:string name)))
            (legacy-name (cl:and (cl:<= 8 (cl:length name-string))
                                 (cl:string= "COALTON/" name-string :end2 8)
                                 (cl:concatenate 'cl:string "COALTON-LIBRARY/"
                                                 (cl:subseq name-string 8)))))
  `(cl:eval-when (:compile-toplevel :load-toplevel)
     (cl:when (cl:find-package ',name)
       (compat:try-unlock-package ',name))
     (cl:defpackage ,name
       ,@(cl:if legacy-name
                `((:nicknames ,legacy-name))
                '())
       ,@args)
     (compat:try-lock-package ',name))))
