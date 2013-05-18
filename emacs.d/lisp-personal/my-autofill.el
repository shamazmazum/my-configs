;; ===================================
;; =========== C-mode autofill========
;; ===================================

(defun return-suggestion (word)
  "Give a suggestion to word"
  (let (keywords elem ret)
    (setq keywords '("while" "if" "break" "continue" "float" "for" "struct"))
    ; List of keywords
    (while keywords
      (setq elem (pop keywords))
      (if (< (length word) (length elem))
	  (progn
	    (if (equal word (substring elem 0 (length word)))
		(setq ret (substring elem (length word)))))))
    ret))


(defun auto-fill nil
  "Auto fill"
  (interactive)
  (let (distspace curpos distnl delta currentword suggestion)
    (setq curpos (point))
    (setq distspace (search-backward " "))
    (setq delta (- curpos distspace))
    (forward-char delta)
    (setq distnl (search-backward "\n"))
    (setq delta (- curpos distnl))
    (forward-char delta)
    (if (> distnl distspace) (setq distspace distnl))
    (setq currentword (buffer-substring-no-properties (+ distspace 1)
						      curpos))
    (setq suggestion (return-suggestion currentword))
    (if (not (equal suggestion nil)) (insert suggestion))))

(defun c-auto-fill-mode nil
  "Main function"
  (interactive)
  (local-set-key (kbd "C-e") 'auto-fill))


(add-hook 'c-mode-hook 'c-auto-fill-mode)

(provide 'my-autofill)