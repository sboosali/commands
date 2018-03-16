;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1. launch runemacs.exe
;; (which gets rid of the background console)
;; 2. test that Dragon can insert into emacs
;; (disabling the dictation box pop up)
;; 3. add this whole file to `init.el`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands/Dragon settings

;; shared folder between the windows guest and the host
(setq shared-folder-directory "F:\\")
;; on the root of the F drive, i.e. F:\

;; the directory that inotifywait is watching
(setq shared-folder-watched-subdirectory "transcriptions")

;; the constantly-saved file that dragon is transcribing into
(setq shared-folder-transcription-file
  (concat shared-folder-directory
          shared-folder-watched-subdirectory "\\"
          "transcription"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; M-x emacs-version
;; e.g.
;; GNU Emacs 25.3.1 (i686-w64-mingw32) of 2017-09-17

;; ;; melpa
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (package-initialize)
;; ;; M-x package-install RET real-auto-save 

;; suppresses obnoxious sights and sounds
(setq visible-bell t)
(setq inhibit-splash-screen t)
(setq initial-scratch-message nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; no tabs indents / no extra newlimes
(setq-default indent-tabs-mode nil)
(setq mode-require-final-newline nil)
(setq require-final-newline nil)

;; encoding
;; (prefer-coding-system 'utf-8)

;; cua
(cua-mode t) ;; C-c, C-x, C-v, C-z
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; remap for single-press M-x
(global-set-key (kbd "<apps>")
		(lookup-key global-map (kbd "M-x")))
(global-set-key (kbd "<lwindow>")
		(lookup-key global-map (kbd "M-x")))

;; ;; save buffers between launches
;; (desktop-save-mode 1)
;; (setq desktop-auto-save-timeout 5) ;; in seconds 

(set-frame-parameter nil 'title "EMACS") 
;; the title of the operating-system-window 
(set-frame-parameter nil 'fullscreen 'maximized)
;; fullwidth, fullheight, fullboth, or maximized

;; increase font size
(set-face-attribute 'default nil :height 400)
;; The value is in 1/10pt, so 100 will give you 10pt, etc.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

(defun milliseconds-to-seconds (time) (/ time 1000.0))
;; casts int-or-float to float

(defun buffer-flush-file-name (&optional b)
 (concat (buffer-file-name b) "")) ;; TODO

(defun buffer-nonempty-p (&optional buffer)
  (> (buffer-size buffer) 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flush-mode

(define-minor-mode flush-mode
  "Flush the current buffer automatically."
  :lighter " Fl"
  :version "0.1"
  :keymap  nil
  :init-value nil

  (when (not flush-mode) ;; OFF
    (disable-flush-mode))

  (when flush-mode ;; ON
    (let ((b (current-buffer)))
      (with-current-buffer b ;; TODO
       (enable-flush-mode b))))
)

(defvar-local flush-mode-local-timer nil 
 "Timer for flush-mode. buffer-local.")

(defvar-local flush-mode-interval 33
 "Interval, in milliseconds, to wait after idleness before flushing in flush-mode. buffer-local.")

;; (defgroup flush nil
;;   "Flush buffers automatically."
;;   :group 'convenience ;; dictation?
;;   :prefix "flush-")

;; (defcustom flush-mode-interval 30
;;   ""
;;   :type 'integer
;;   :group 'flush)

(defun enable-flush-mode (b)
   "enable flush-mode on the buffer `b`. 
    it's idempotent."

   ;; start empty
   (erase-buffer)
   (write-file (buffer-file-name b) nil)
   ;; (doesn't create file if nonexistant, until save/write)
   ;; force write, no prompt
   ;; (write-file FILENAME &optional CONFIRM)

   (let
     ((old-timer 
       flush-mode-local-timer)

      (new-timer
       (run-with-idle-timer 
        (milliseconds-to-seconds flush-mode-interval) 
        t
        'flush-buffer-to-file
        b))) ;; lexically-scoped

     (progn
      ;; only one timer at a time
      (when old-timer
        (cancel-timer old-timer))
      (setq flush-mode-local-timer
            new-timer))))

(defun disable-flush-mode ()
   "disable flush-mode on the current buffer".
   (let
     ((the-timer 
       flush-mode-local-timer))
     (when the-timer
       (cancel-timer the-timer))))

;; define-minor-mode
;; The toggle command takes one optional (prefix) argument. 
;; If called interactively with no argument it toggles the mode on or off. A positive prefix argument enables the mode, any other prefix argument disables it.
;; From Lisp, an argument of toggle toggles the mode, whereas an omitted or nil argument enables the mode. 

;; :lighter: the name, a string, to show in the modeline
;; :keymap: the mode’s keymap
;; The :lighter option has one caveat: it’s concatenated to the rest of the modeline without any delimiter. This means it needs to be prefixed with a space.

;; The rest of define-minor-mode is a body for arbitrary Lisp, like a defun. It’s run every time the mode is toggled off or on, so it’s like a built-in hook function. Use it to do any sort of special setup or teardown, such hooking or unhooking Emacs’ hooks. A likely thing to be done in here is specifying buffer-local variables.
;; Any time the Emacs interpreter is evaluating an expression there’s always a current buffer acting as context. Many functions that operate on buffers don’t actually accept a buffer as an argument. Instead they operate on the current buffer. Furthermore, some variables are buffer-local: the binding is dynamic over the current buffer. This is useful for maintaining state relevant only to a particular buffer.
;; Side note: the with-current-buffer macro is used to specify a different current buffer for a body of code. It can be used to access other buffer’s local variables. Similarly, with-temp-buffer creates a brand new buffer, uses it as the current buffer for its body, and then destroys the buffer.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; for flush-mode

;; for transcribing
(defun start-transcribing (filename)
 "call `flush-mode` on whitelisted `filename`."
 (auto-save-mode 0) ;; disable
 (find-file filename)
 (flush-mode))

 ;; (when (eq filename
 ;;           (buffer-file-name (current-buffer)))
 ;;    (flush-mode))) ;; no/nil arg to mode enables unconditionally
;; (start-transcribing shared-folder-transcription-file)

(defun flush-buffer-to-file (&optional b)
 (interactive)

 (let ((b-name (buffer-flush-file-name b)))

   (if (and (buffer-live-p     b)  ;; exists and hasn't been killed
            (buffer-nonempty-p b)  ;; 
            (buffer-file-name  b)  ;; non-nil filename, i.e. whether the buffer is visiting a file
            (buffer-modified-p b)  ;; 
            (eq b (window-buffer (selected-window)))) ;; Visible and focused

   (progn
     (message "[Flushing '%s']" b-name)
     (write-file b-name nil) 
     ;; save chunk to new file, one new file per single utterance
     ;; TODO an 'utterance' is defined by the idle time threshold, in run-with-idle-timer
     (with-current-buffer b 
        (erase-buffer)))
     ;; Delete the entire contents of the ***current*** buffer.
      ;; save-buffer versus write-file?

   (progn
     (message "")))))
      ;; clear echo to show minibuffer
     ;; debug (message "[NOT flushing '%s']" b-name)))))

;;NOTE erase-buffer
; Delete the entire contents of the ***current*** buffer.

;;NOTE message
;If the first argument is nil or the empty string, the function clears any existing message; this lets the minibuffer contents show.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EFFECTS

;; for convenience...
;; init.el
;; user-init-file is .emacs
;; user-emacs-directory is:
;; c:/Users/IEUser/AppData/Roaming/.emacs.d/.emacs.d/
(find-file
 (concat user-emacs-directory "init.el"))

;; for forwarding recognitions...
(start-transcribing
 shared-folder-transcription-file)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;