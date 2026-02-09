(uiop:define-package #:coalton/math
  (:nicknames
   #:coalton-library/math)
  (:use
   #:coalton-compatibility)
  (:use-reexport
   #:coalton/math/arith
   #:coalton/math/num
   #:coalton/math/bounded
   #:coalton/math/conversions
   #:coalton/math/fraction
   #:coalton/math/integral
   #:coalton/math/real
   #:coalton/math/complex
   #:coalton/math/elementary
   #:coalton/math/dual))

(coalton-compatibility:try-lock-package "COALTON/MATH")
