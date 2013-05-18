;;;
;;; Alexey Khudyakov
;;; Abbreviations
;;;

;; =========================================================
;; C/C++ abbreviations
;; =========================================================
(define-skeleton my-skel-reintepret-cast
  "Skeleton for reinterpret cast"
  "Typename: "
  "reinterpret_cast<" str ">(" _ ")")
(define-skeleton my-skel-static-cast
  "Skeleton for static cast"
  "Typename: "
  "static_cast<" str ">(" _ ")")
(define-skeleton my-skel-printf
  "Skeleton for printf" ""
  "printf(\"" _ "\");" )
  
(define-abbrev-table 'c-mode-abbrev-table 
  '( 
    ; Preprocessor directives 
;    ("inc" "#include"  nil 0)
 ;   ("def" "#define"   nil 0)
    ; Control structures
    ("sw"   "switch"   nil 0)
    ("ca"   "case"     nil 0)
    ("br"   "break"    nil 0)
    ("wh"   "while"    nil 0)
    ("ret"  "return"   nil 0)
    ("cont" "continue" nil 0)
    ; Types and modifiers 
    ("co" "const"      nil 0)
    ("vo" "void"       nil 0)
    ("bo" "bool"       nil 0)
    ; printf
    ("pr" ""           my-skel-printf 0)
    ))
(define-abbrev-table 'c++-mode-abbrev-table
  '(
    ; Preprocessor directives 
    ("inc" "#include"  nil 0)
    ("def" "#define"   nil 0)
    ; Control structures
    ("sw"   "switch"   nil 0)
    ("ca"   "case"     nil 0)
    ("br"   "break"    nil 0)
    ("wh"   "while"    nil 0)
    ("ret"  "return"   nil 0)
    ("cont" "continue" nil 0)
    ; Types and modifiers 
    ("co" "const"      nil 0)
    ("vo" "void"       nil 0)
    ("bo" "bool"       nil 0)
    ; Classes
    ("pub"  "public"   nil 0)
    ("priv" "private"  nil 0)
    ; printf
    ("pr" ""           my-skel-printf 0)
    ; Casts
    ("rcast" ""       my-skel-reintepret-cast 0)
    ("scast" ""       my-skel-static-cast 0)
    ; STL
    ("cout"  "std::cout" nil 0)
    ("cerr"  "std::cerr" nil 0)
    ("endl"  "std::endl" nil 0)
    ))
;; =================


;; =========================================================
;; Python abbreviations
;; =========================================================
(define-skeleton my-skel-python-import-all
  "``Import all'' clause"
  "Import from: "
  "from " str " import *" _ "")

(define-skeleton my-skel-python-range
  "skeleton for python ``range''"
  "Range: "
  "range(" str ")" _ )

(define-abbrev-table 'python-mode-abbrev-table
  '(
    ; import clause  
    ("imp"   "import" nil 0)
    ("impa"  ""       my-skel-python-import-all 0)
    ("ra"    ""       my-skel-python-range      0)
    ; Return
    ("ret"   "return" nil 0)
    ))
;; =================

;; =========================================================
;; Haskell abbreviations
;; =========================================================
(define-skeleton my-skel-haskell-SCC
  "Skeleton for haskell SCC pragma"
  "SCC name: "
  "{-# SCC \"" str "\" #-}" _
  )
(define-skeleton my-skel-haskell-LANG
  "Skeleton for haskell LANGUAGE pragma"
  ""
  "{-# LANGUAGE " _ " #-}" 
  )
(define-abbrev-table 'haskell-mode-abbrev-table
  '(
    ; Import
    ("imp"  "import"    nil 0)
    ("qua"  "qualified" nil 0)
    ; Shorhands
    ("ret"  "return"    nil 0)
    ("pr"   "print"     nil 0)
    ("pscc"  ""         my-skel-haskell-SCC  0)
    ("plang" ""         my-skel-haskell-LANG 0)
    ))

(provide 'my-abbrevs)
