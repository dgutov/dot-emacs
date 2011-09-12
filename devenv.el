(update-load-path-vc "eproject")
(update-load-path-vc "auto-complete")
(update-load-path-vc "ac-slime")
(update-load-path-vc "pos-tip")
(update-load-path-vc "anything-config")
(update-load-path-vc "anything-config/extensions")
(update-load-path-vc "point-stack")
(update-load-path-vc "magit" t)
(update-load-path-vc "markdown-mode")

(require 'eproject)
(require 'eproject-extras)
(require 'auto-complete-config)
(require 'ac-slime)
(require 'pos-tip)
(require 'point-stack)

(define-project-type make (generic) (look-for "Makefile"))
(define-project-type rake (generic) (look-for "Rakefile"))
(define-project-type lein (generic) (look-for "project.clj"))
(define-project-type gae (generic) (look-for "app.yaml"))
(define-project-type scons (generic) (look-for "SConstruct"))
(define-project-type ant (generic) (look-for "build.xml"))
(define-project-type haskell (generic) (look-for "Setup.hs"))
(define-project-type emacs (generic) (look-for "init.el")
  :irrelevant-files ("^[.]" "/elpa/" "/url/cookies$" "tramp$" "^custom.el$"))

(add-auto-mode 'python-mode "SConstruct" "SConscript")
(add-auto-mode 'markdown-mode "\\.md$")

(add-lambda 'scons-project-file-visit-hook
  (setq-local compile-command (format "cd %s && scons" (eproject-root))))

(add-to-list 'ac-dictionary-directories (get-vc-dir "auto-complete/dict"))

(ac-config-default)

(setq ac-quick-help-delay 0.2
      ac-use-comphist nil
      ac-quick-help-prefer-x t
      ac-auto-start nil
      tab-always-indent 'complete)

(set-face-attribute 'popup-tip-face nil
                    :background pos-tip-background-color)

(put 'dropdown-list-face 'face-alias 'popup-menu-face)
(put 'dropdown-list-selection-face 'face-alias 'popup-menu-selection-face)

(add-lambda 'slime-mode-hook
  (setq-local ac-sources '(ac-source-slime-simple
                           ac-source-words-in-same-mode-buffers
                           ac-source-filename)))

(add-lambda 'haskell-mode-hook
  (setq-local ac-sources '(ac-source-ghc-mod
                           ac-source-words-in-same-mode-buffers)))

(add-lambda 'auto-complete-mode-hook
  (setq completion-at-point-functions '((lambda () #'auto-complete))))

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

(defun anything-in-project ()
  (interactive)
  (require 'anything-config)
  (require 'anything-match-plugin)
  (if eproject-mode
      (anything '(anything-c-project-buffers
                  anything-c-project-files
                  anything-c-nonproject-buffers
                  anything-c-nonproject-recentf))
    (anything-not-in-project)))

(defun anything-not-in-project ()
  (interactive)
  (require 'anything-config)
  (require 'anything-match-plugin)
  (anything '(anything-c-nonproject-buffers
              anything-c-nonproject-recentf)))

(defun anything-imenu-thingatpt ()
  (interactive)
  (require 'anything-config)
  (let ((input (thing-at-point 'symbol)))
    (let ((anything-quit-if-no-candidate
           (and input (lambda ()
                        (setq anything-quit nil
                              anything-pattern "")
                        (anything-update)
                        (let ((anything-reading-pattern t))
                          (read-string "pattern: " nil))))))
      (anything 'anything-c-source-imenu input nil nil nil "*anything imenu*"))))

(defun anything-etags-select-thingatpt (clear-cache)
  (interactive "P")
  (require 'anything-config)
  (let* ((tag (anything-c-etags-get-tag-file))
         (symbol (with-syntax-table (standard-syntax-table)
                   (thing-at-point 'symbol)))
         (init (when symbol (format "\\_<%s\\_>\\([?!(\\ ]\\|$\\)"
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

(autoload 'anything-etags-select-from-here "anything-etags.el" nil t)

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

(defadvice* point-stack-push before (anything-c-etags-default-action
                                     imenu find-function isearch-mode)
  (point-stack-push))

(defun ecb-add-project-to-sources (&optional root)
  (let ((root (or root eproject-root)))
    (ecb-add-source-path root
                         (car (last (split-string root "/" t)))
                         t)))

(defun ecb-hook-eproject ()
  (let ((known-paths (mapcar 'car (ecb-normed-source-paths))))
    (dolist (root (eproject--known-project-roots))
      (unless (member (directory-file-name root) known-paths)
        (ecb-add-project-to-sources root))))
  (add-hook 'eproject-first-buffer-hook 'ecb-add-project-to-sources))

(defun ecb-unhook-eproject ()
  (remove-hook 'eproject-first-buffer-hook 'add-project-to-ecb-sources))

(add-hook 'ecb-before-activate-hook 'ecb-hook-eproject)
(add-hook 'ecb-deactivate-hook 'ecb-unhook-eproject)

(provide 'devenv)
