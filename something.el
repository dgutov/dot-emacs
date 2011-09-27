(update-load-path-vc "anything-config")
(update-load-path-vc "anything-config/extensions")

(require 'anything-config)
(require 'anything-match-plugin)

(setq anything-c-project-files
      '((name . "Project Files")
        (candidates . anything-project-files-candidates)
        (candidate-transformer . anything-project-files-transformer)
        (type . file)))

(defvar not-in-project-p nil)

(defface anything-ip-file
  '((t (:foreground "Blue"))) "")

(defun anything-project-files-candidates ()
  (with-current-buffer anything-current-buffer
    (mapcar (lambda (file)
              (cons (file-relative-name file eproject-root) file))
            (eproject-list-project-files))))

(defun anything-project-skip-buffers (buffers)
  (with-current-buffer anything-current-buffer
    (let ((project-root eproject-root))
      (loop for buffer in buffers
            when (with-current-buffer buffer
                   (xor not-in-project-p
                        (and eproject-root
                             (equal eproject-root project-root))))
            collect buffer))))

(defun anything-skip-project-buffers (buffers)
  (let ((not-in-project-p t))
    (anything-project-skip-buffers buffers)))

(defun anything-project-files-transformer (files)
  (nreverse
   (mapcar (lambda (i)
             (cons (propertize (car i) 'face 'anything-ip-file)
                   (cdr i)))
           files)))

(defun anything-skip-project-recentf (files)
  (with-current-buffer anything-current-buffer
    (if eproject-root
        (loop for file in files
              when (not (string-prefix-p eproject-root file))
              collect file)
      files)))

;;;###autoload
(defun anything-in-project ()
  (interactive)
  (if eproject-mode
      (anything '(anything-c-project-buffers
                  anything-c-project-files
                  anything-c-nonproject-buffers
                  anything-c-nonproject-recentf))
    (anything-not-in-project)))

;;;###autoload
(defun anything-not-in-project ()
  (interactive)
  (anything '(anything-c-nonproject-buffers
              anything-c-nonproject-recentf)))

;;;###autoload
(defun anything-imenu-thingatpt ()
  (interactive)
  (let ((input (thing-at-point 'symbol)))
    (let ((anything-quit-if-no-candidate
           (and input (lambda ()
                        (setq anything-quit nil
                              anything-pattern "")
                        (anything-update)
                        (let ((anything-reading-pattern t))
                          (read-string "pattern: " nil))))))
      (anything 'anything-c-source-imenu input nil nil nil "*anything imenu*"))))

;;;###autoload
(defun anything-etags-select-thingatpt (clear-cache)
  (interactive "P")
  (let* ((tag (anything-c-etags-get-tag-file))
         (symbol (with-syntax-table (standard-syntax-table)
                   (thing-at-point 'symbol)))
         (init (when symbol (format "\\_<%s\\_>\\ *\\([?!(<]\\|$\\)"
                                    (regexp-quote symbol))))
         (anything-quit-if-no-candidate
          (lambda () (message "Tag '%s' not found." symbol)))
         (anything-execute-action-at-once-if-one t))
    (when (or clear-cache
              (and anything-c-etags-mtime-alist
                   (anything-c-etags-file-modified-p tag)))
      (remhash tag anything-c-etags-cache))
    (if (and tag (file-exists-p tag))
        (anything :sources 'anything-c-source-etags-select
                  :keymap anything-c-etags-map
                  :input init
                  :buffer "*anything etags*")
        (message "Tags file not found."))))

(eval-after-load 'anything-config
  '(progn
     (define-key anything-map (kbd "C-z") nil) ; hide from persistent help
     (define-key anything-map (kbd "C-;") 'anything-execute-persistent-action)
     
     (defvar anything-c-project-buffers (copy-alist anything-c-source-buffers-list))
     (setcdr (assoc 'candidate-transformer anything-c-project-buffers)
             '(anything-c-skip-current-buffer
               anything-c-highlight-buffers
               anything-project-skip-buffers))

     (defvar anything-c-nonproject-buffers (copy-alist anything-c-source-buffers-list))
     (setcdr (assoc 'candidate-transformer anything-c-nonproject-buffers)
             '(anything-c-skip-current-buffer
               anything-c-highlight-buffers
               anything-skip-project-buffers
               anything-c-skip-boring-buffers))
     (setcdr (assoc 'name anything-c-nonproject-buffers) "Misc Buffers")

     (defvar anything-c-nonproject-recentf
       (cons '(candidate-transformer . anything-skip-project-recentf)
             anything-c-source-recentf))))

(provide 'something)
