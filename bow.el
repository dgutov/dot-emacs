(require 'helm-config)
(require 'helm-match-plugin)
(require 'helm-buffers)
(provide 'helm-bookmark) ; never used that
(require 'helm-files)

(defvar helm-project-files
  '((name . "Project Files")
    (candidates . helm-project-files-candidates)
    (candidate-transformer . helm-project-files-transformer)
    (type . file)))

(defvar not-in-project-p nil)

(defface helm-ip-file
  '((t (:foreground "Blue"))) "")

(defun helm-project-files-candidates ()
  (with-current-buffer helm-current-buffer
    (mapcar (lambda (file)
              (cons (file-relative-name file eproject-root) file))
            (eproject-list-project-files))))

(defun helm-project-skip-buffers (buffers)
  (with-current-buffer helm-current-buffer
    (let ((project-root eproject-root))
      (loop for buffer in buffers
            when (with-current-buffer buffer
                   (xor not-in-project-p
                        (and eproject-root
                             (equal eproject-root project-root))))
            collect buffer))))

(defun helm-skip-project-buffers (buffers)
  (let ((not-in-project-p t))
    (helm-project-skip-buffers buffers)))

(defun helm-project-files-transformer (files)
  (nreverse
   (mapcar (lambda (i)
             (cons (propertize (car i) 'face 'helm-ip-file)
                   (cdr i)))
           files)))

(defun helm-skip-project-recentf (files)
  (with-current-buffer helm-current-buffer
    (if eproject-root
        (loop for file in files
              when (not (string-prefix-p eproject-root file))
              collect file)
      files)))

;;;###autoload
(defun helm-in-project ()
  (interactive)
  (if eproject-mode
      (helm '(helm-project-buffers
              helm-project-files
              helm-nonproject-buffers
              helm-nonproject-recentf))
    (helm-not-in-project)))

;;;###autoload
(defun helm-not-in-project ()
  (interactive)
  (helm '(helm-nonproject-buffers
          helm-nonproject-recentf)))

;;;###autoload
(defun helm-imenu-thingatpt ()
  (interactive)
  (require 'helm-imenu)
  (let ((input (thing-at-point 'symbol)))
    (let ((helm-quit-if-no-candidate
           (and input (lambda ()
                        (setq helm-quit nil
                              helm-pattern "")
                        (helm-update)
                        (let ((helm-reading-pattern t))
                          (read-from-minibuffer "pattern: " nil helm-map))))))
      (helm :sources 'helm-c-source-imenu :buffer "*helm imenu*"
            :input input))))

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

(eval-after-load 'helm-config
  '(progn
     (define-key helm-map (kbd "C-z") nil) ; hide from persistent help
     (define-key helm-map (kbd "C-;") 'helm-execute-persistent-action)

     (defvar helm-project-buffers
       (append (copy-alist helm-c-source-buffers-list)
               '((candidate-transformer helm-project-skip-buffers
                                        helm-c-highlight-buffers))))

     (defvar helm-nonproject-buffers
       (append (copy-alist helm-c-source-buffers-list)
               '((candidate-transformer helm-skip-project-buffers
                                        helm-c-skip-boring-buffers
                                        helm-c-highlight-buffers))))
     (setcdr (assoc 'name helm-nonproject-buffers) "Misc Buffers")

     (defvar helm-nonproject-recentf
       (cons '(candidate-transformer . helm-skip-project-recentf)
             helm-c-source-recentf))

     (defvar helm-etags-thingatpt
       (copy-alist helm-c-source-etags-select))
     (setcdr (assoc 'init helm-etags-thingatpt) 'helm-etags-init-with-syntax)))

(provide 'bow)
