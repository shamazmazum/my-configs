;; You are free to do with this code what you will

;; Summary:
;; 1) You can build your own program launcher redefining *key-bindings*
;; C-t C-r h is a help on the program launcher
;; C-t C-r m is an interactive launcher menu
;;
;; 2) You can close a window with C-t d and close all windows with C-t ]
;; Also there is "safe quit" command accessible with C-t q
;; It makes stumpwm to quit only if there are no windows left

;; 3) You can set a wallpaper from wallpaper collection via interactive menu
;; with C-t .

;; 4) Path to current wallpaper will be stored in preferences file
;; and the wallpaper will restored in next session

;; WARNING: This configuration file uses *quit-hook*, which is absent in
;; stumpwm distribution(s) and is added to stumpwm by myself.
;; If you want preferences to be stored as stumpwm quits
;; add this patch to stumpwm

#|
diff --git a/primitives.lisp b/primitives.lisp
index 50d062e..96ffa58 100644
--- a/primitives.lisp
+++ b/primitives.lisp
@@ -40,6 +40,7 @@
           *focus-window-hook*
           *place-window-hook*
           *start-hook*
+          *quit-hook*
           *internal-loop-hook*
           *focus-frame-hook*
           *new-frame-hook*
@@ -190,6 +191,9 @@ window group and frame")
 (defvar *start-hook* '()
   "A hook called when stumpwm starts.")
 
+(defvar *quit-hook* '()
+  "A hook called when stumpwm quits.")
+
 (defvar *internal-loop-hook* '()
   "A hook called inside stumpwm's inner loop.")
 
diff --git a/stumpwm.lisp b/stumpwm.lisp
index 845c745..65d4ba7 100644
--- a/stumpwm.lisp
+++ b/stumpwm.lisp
@@ -267,4 +267,5 @@ of those expired."
              ((eq ret :restart))
              (t 
               ;; the number is the unix return code
+              (run-hook *quit-hook*)
               (return-from stumpwm 0))))))
|#


(if *initializing*
    (asdf:load-system :cl-fad))

(defparameter *shell* "xterm -e ")
(defparameter *wallpapers-dir* "/home/vasily/.e16/backgrounds"
  "It contains a path to all my wallpapers")
;; File with preferences
(defparameter *preferences-fn* "~/.stumpwm.pref"
  "File in which preferences will be stored")
(defvar *preferences* nil
  "Preferences are used to store data between sessions")

;; Preferences management
(defun load-pref ()
  "Loads preferences from file"
  (handler-case
   (with-open-file (stream *preferences-fn*
			   :direction :input)
		   (setq *preferences* (read stream)))
   (file-error () (message "You have no preferences file :("))))

(defun store-pref ()
  "Stores preferences to file"
  (with-open-file (stream *preferences-fn*
			  :direction :output
			  :if-does-not-exist :create
			  :if-exists :supersede)
		  (write *preferences* :stream stream)))

(defun set-pref (key value)
  "Stores key-value pair in preferences"
  (let ((old-pref (assoc key *preferences*)))
    (if old-pref
	(setf (cdr old-pref) value)
      (push (cons key value) *preferences*))))

;; Restore previous wallpaper if any
(add-hook *start-hook* #'(lambda ()
			   (let ((prev-wp (assoc :wallpaper *preferences*)))
			     (if prev-wp (set-wallpaper (cdr prev-wp))))))

;; Load preferences at start and store at quit
(add-hook *start-hook* #'load-pref)
(add-hook *quit-hook* #'store-pref)

;; Program launcher
(defvar *launcher-bindings* (make-sparse-keymap))
(defvar *key-bindings* '(("f" . "firefox")
			 ("t" . "uxterm")
			 ("e" . "evince")
			 ("g" . "gimp"))
  "Bindings for launcher")

(defun make-bindings (&optional (bindings *key-bindings*))
  "Defines launcher commands and key bindings"
  (loop for binding in bindings do
	(destructuring-bind (key . prog-name) binding
	  (let ((symbol (intern (string-upcase prog-name)))
		(caller #'(lambda ()
			    (run-shell-command prog-name))))
	    (setf (symbol-function symbol) caller
		  (gethash symbol stumpwm::*command-hash*)
		  (stumpwm::make-command :name symbol :class t)))
	  
	  (define-key *launcher-bindings* (kbd key) prog-name))))

(make-bindings *key-bindings*)

(defcommand launcher-help (&optional (bindings *key-bindings*)) ()
  (message
   (apply #'concatenate 'string "Use the following bindings to launch programs:~%"
	  (loop for binding in bindings collect
		(format nil "~A - to run ~A~%" (car binding) (cdr binding))))))
(define-key *launcher-bindings* (kbd "h") "launcher-help")

(defcommand launcher-menu (&optional (bindings *key-bindings*)) ()
  (let* ((options (loop for binding in bindings
			for prog-name = (cdr binding) collect
			(list prog-name prog-name)))
	 
	 (selection (stumpwm::select-from-menu
		     (current-screen)
		     options
		     "Select a program to run")))

    (if (null selection) (throw 'stumpwm::error "Abort."))
    (run-commands (second selection))))

(define-key *launcher-bindings* (kbd "m") "launcher-menu")
(define-key *root-map* (kbd "C-r") *launcher-bindings*)
(define-key *root-map* (kbd "d") "delete-window")

(defcommand exec-in-shell (command) ((:string "Command to run in shell: "))
  (run-shell-command (concatenate 'string *shell* command)))
(define-key *root-map* (kbd "x") "exec-in-shell")

;; Safe quit

(defcommand safe-quit () ()
  (loop for group in (stumpwm::screen-groups (current-screen)) do
	(if (/= 0 (length (stumpwm::group-windows group))) (throw 'stumpwm::error "You must close all windows first")))
  (run-commands "quit"))
(define-key *root-map* (kbd "q") "safe-quit")

;; Close all windows (so you can quit safely)
(defcommand delete-all () ()
  (loop for group in (stumpwm::screen-groups (current-screen)) do
	(loop for window in (stumpwm::group-windows group) do
	      (stumpwm::delete-window window))))
(define-key *root-map* (kbd "]") "delete-all")

;; Wallpaper (why not?)

(defun set-wallpaper (name)
  "Sets wallpaper with help of xli"
  (run-shell-command
   (format nil "xli -onroot -fullscreen ~A" name))
  (set-pref :wallpaper name))

(defcommand choose-wallpaper (&optional (dir *wallpapers-dir*)) ()
  (labels ((filter (sequence predicate &optional res)
		   (if (null sequence) res
		     (let ((head (car sequence)))
		       (filter (cdr sequence)
			       predicate
			       (if (funcall predicate head)
				   (cons head res)
				 res))))))
    
    (let* ((files (cl-fad:list-directory dir))
	   (pictures (filter files
			     #'(lambda (file)
				 (let ((file-type (pathname-type file)))
				   (or (string= "jpg" file-type)
				       (string= "png" file-type)
				       (string= "bmp" file-type))))))
	   
	   (menu (mapcar #'(lambda (file)
			     (list (file-namestring file) file)) pictures))

	   (selection (stumpwm::select-from-menu
		       (current-screen)
		       menu
		       "Select a wallpaper")))

      (if (null selection) (throw 'stumpwm::error "Abort."))
      (set-wallpaper (namestring (second selection))))))

(define-key *root-map* (kbd ".") "choose-wallpaper")
