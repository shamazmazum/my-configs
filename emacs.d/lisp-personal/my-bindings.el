;;;
;;; Alexey Khudyakov
;;; Global key bindings
;;;

;; =========================================================
;; F# keys shortcuts 
;; =========================================================
; Move between windows quickly quickly
(global-set-key [(shift up)]  
                (lambda () (interactive) (other-window 1)) )
(global-set-key [(shift down)] 
                (lambda () (interactive) (other-window -1)) )
; Close curent buffer
(global-set-key [f10]            
                (lambda () (interactive) (kill-buffer (current-buffer))) )
; Finish editing when acting as server
(global-set-key [f9] 'server-edit)
; Swith to buffer (same as 'C-x b')
(global-set-key [f12] 'switch-to-buffer)
; Call last recorded macro
(global-set-key [f2] 'call-last-kbd-macro)

;; =========================================================
;; kill-yank
;; =========================================================
; Replace kill-region with backward-kill-word
(global-set-key "\C-w" 'backward-kill-word)
; Set kill-region to "C-x C-k"
(global-set-key "\C-x\C-k" 'kill-region)

;; =========================================================
;; Programming and identation 
;; =========================================================
; Autoidentation of source code 
(global-set-key [\C-tab] 'my-indent-line)
; Find matching paren
(global-set-key (kbd "C-%") 'my-match-paren)
; Insert tab
(global-set-key [backtab]
		(lambda() (interactive) (insert "	")))

(provide 'my-bindings)