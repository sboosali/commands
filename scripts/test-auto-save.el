
;; auto-save-hook
;;
;; which checks both: 
;; an idle timer; 
;; and a keystroke counter;
;; whichever's sooner. 
;;
;; called before performing the auto-save
(defun run-whenever-idle (f)  
  (add-hook 'auto-save-hook f))

;; there is no after-auto-save-hook
;; 
;; there are:
;; post-self-insert-hook
;; before-revert-hook
;; before-save-hook

(defun flush-buffer-to-file ()
 (interactive)
 (if (buffer-modified-p)
   (progn
     (if (buffer-file-name) ;; whether the buffer is visiting a file
         (write-file (buffer-file-name) nil)) ;; force-save it, i.e. no confirmation
     (erase-buffer) ;; Delete the entire contents of the current buffer.
   )))
      ;; save-buffer versus write-file?

;; TODO real minor mode
; (defun flush--mode ()
; )
  
(defun flush-pseudo-mode ()
 (interactive)

 ;; autosave inplace
 (setq auto-save-visited-file-name t)
 
 ;; Each time the user pauses for this long.
 ;; In seconds.
 ;; (setq auto-save-timeout 0.030) ;; 30ms
 (setq auto-save-timeout 1) ;; 
 
 ;; On every character inserted. (and every other "event", i.e. ...?)
 ;; The minimum.
 ;; (setq auto-save-interval 1)

 ;; add-hook is idempotent
 (add-hook 'auto-save-hook 'flush-buffer-to-file nil t)
 ;; (add-hook HOOK FUNCTION &optional APPEND LOCAL)

 (auto-save-mode)
)

;; (defun f ()
;;  (if (buffer-modified-p)
;;    (progn
;;      (save-buffer)     
;;    )))

;; TODO make idempotent
;; (run-whenever-idle 'flush-buffer-to-file)
;; (remove-hook 'auto-save-hook 'flush-buffer-to-file) 
