;;;
;;; Alexey Khudyakov
;;; Main emacs configuration file 
;;;

;; Enable a backtrace when problems occur
;; (setq debug-on-error t)

;; Load paths
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))

;; Set font
(set-default-font "Terminus-12")

; Require local modification (if any)
(require 'my-local "my-local.el" t)

; Load files
(require 'my-generic)
(require 'my-text)
(require 'my-programming)
(require 'my-abbrevs)

(require 'my-bindings)
(require 'my-appearance)
(require 'my-autofill)

; Other stuff
(require 'w3m)
(require 'jabber)
