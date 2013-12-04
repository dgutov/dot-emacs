(defun helm-etags-init-with-syntax ()
  (let ((table (syntax-table)))
    (helm-c-etags-init)
    (with-current-buffer (helm-candidate-buffer)
      (set-syntax-table table))))

;;;###autoload
(defun helm-etags-select-thingatpt (clear-cache)
  (interactive "P")
  (require 'helm-tags)
  (let* ((tag (helm-c-etags-get-tag-file))
         (symbol (thing-at-point 'symbol))
         (input (when symbol (format "\\_<%s\\_>" (regexp-quote symbol))))
         (helm-quit-if-no-candidate
          (lambda () (message "Tag '%s' not found." symbol)))
         (helm-execute-action-at-once-if-one t))
    (when (or clear-cache
              (and helm-c-etags-mtime-alist
                   (helm-c-etags-file-modified-p tag)))
      (remhash tag helm-c-etags-cache))
    (if (and tag (file-exists-p tag))
        (helm :sources 'helm-etags-thingatpt
              :keymap helm-c-etags-map
              :input input
              :buffer "*helm etags*")
      (message "Tags file not found."))))

(eval-after-load 'helm
  '(progn
     (require 'helm-tags)

     (define-key helm-map (kbd "C-z") nil) ; hide from persistent help
     (define-key helm-map (kbd "C-;") 'helm-execute-persistent-action)

     (defvar helm-etags-thingatpt
       (copy-alist helm-source-etags-select))
     (setcdr (assoc 'init helm-etags-thingatpt) 'helm-etags-init-with-syntax)

     (set-face-attribute 'helm-selection nil
                         :background "cornsilk" :underline nil)))

(provide 'bow)
