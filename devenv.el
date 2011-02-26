(update-load-path-vc "eproject")
(update-load-path "~/emacs-ac")
(update-load-path-vc "ac-slime")
(update-load-path-vc "pos-tip")
(update-load-path-vc "anything-config")
(update-load-path-vc "anything-config/extensions")
(update-load-path-vc "point-stack")

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
  :irrelevant-files ("^[.]" "/elpa/" "/site-lisp/"
                     "/url/cookies$" "tramp$" "^custom.el$"))

(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))
(add-to-list 'auto-mode-alist '("SConscript" . python-mode))

(add-hook 'scons-project-file-visit-hook
          (lambda ()
            (setq-local compile-command (format "cd %s && scons" (eproject-root)))))

(add-to-list 'ac-dictionary-directories "~/emacs-ac/ac-dict")

(ac-config-default)

(setq ac-quick-help-delay 0.2
      ac-use-comphist nil
      ac-quick-help-prefer-x t
      ac-auto-start nil)

(set-face-attribute 'popup-tip-face nil
                    :background pos-tip-background-color)

(add-hook 'slime-mode-hook
          (lambda () (setq-local ac-sources
                            '(ac-source-slime-simple
                              ac-source-words-in-same-mode-buffers
                              ac-source-filename))))

(add-hook 'haskell-mode-hook
          (lambda () (setq-local ac-sources
                            '(ac-source-ghc-mod
                              ac-source-words-in-same-mode-buffers))))

(setq anything-c-project-files
      '((name . "Project Files")
        (candidates . anything-project-files-candidates)
        (candidate-transformer . anything-project-files-transformer)
        (type . file)))

(defun anything-project-files-candidates ()
  (with-current-buffer anything-current-buffer
    (let ((dir (if eproject-mode eproject-root default-directory)))
      (mapcar (lambda (file)
                  `(,(file-relative-name file dir) . ,file))
              (if eproject-mode
                  (eproject-list-project-files)
                (if (buffer-file-name)
                    (directory-files dir nil "^[^._]")))))))

(defun anything-project-files-transformer (files)
  (nreverse
   (mapcar (lambda (i)
             `(,(propertize (car i)
                            'face (if (file-directory-p (cdr i))
                                      anything-c-files-face1
                                    anything-c-files-face2))
               . ,(cdr i)))
           files)))

(defun anything-in-project ()
  (interactive)
  (require 'anything-config)
  (anything '(anything-c-source-buffers+
              anything-c-project-files
              anything-c-source-recentf)))

(defun anything-imenu-discard-bad-input ()
  "If the input has no matches, deletes the input and displays all candidates."
  (remove-hook 'anything-after-update-hook 'anything-imenu-discard-bad-input)
  (when (= 1 (point-max)) ; candidates buffer is empty
    ;; `any-input' is function argument in `anything-read-pattern-maybe'
    ;; dynamic scoping is evil
    (setq any-input nil)
    (setq anything-pattern "")
    (anything-update)))

(defun anything-imenu-thingatpt ()
  (interactive)
  (require 'anything-config)
  (let ((input (thing-at-point 'symbol)))
    (if input
        (add-hook 'anything-after-update-hook 'anything-imenu-discard-bad-input))
    (anything 'anything-c-source-imenu input nil nil nil "*anything imenu*")))

(eval-after-load 'anything-config
  '(progn
     (define-key anything-map (kbd "C-z") nil) ; hide from persistent help
     (define-key anything-map (kbd "C-;") 'anything-execute-persistent-action)))

(defadvice* point-stack-push before (imenu find-function isearch-mode)
  (point-stack-push))

(provide 'devenv)
