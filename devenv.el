(update-load-path-vc "eproject")
(update-load-path-vc "auto-complete")
(update-load-path-vc "ac-slime")
(update-load-path-vc "pos-tip")
(update-load-path-vc "point-stack")
(update-load-path-vc "markdown-mode")
(update-load-path-vc "ethan-wspace/lisp")
(update-load-path-vc "helm")
(update-load-path-vc "helm-descbinds" t)

(require 'eproject)
(require 'eproject-extras)
(require 'auto-complete-config)
(require 'ac-slime)
(require 'pos-tip)
(require 'point-stack)
(require 'ethan-wspace)

(define-project-type make (generic) (look-for "Makefile"))
(define-project-type rake (generic) (look-for "Rakefile"))
(define-project-type lein (generic) (look-for "project.clj"))
(define-project-type gae (generic) (look-for "app.yaml"))
(define-project-type ant (generic) (look-for "build.xml"))
(define-project-type haskell (generic) (look-for "Setup.hs"))
(define-project-type emacs (generic) (look-for "init.el")
  :irrelevant-files ("/elpa/" "/url/cookies$" "tramp$" "/server/"
                     "^custom.el$" "^places$" "/backups/"))

(add-lambda 'scons-project-file-visit-hook
  (setq-local compile-command (format "cd %s && scons" (eproject-root))))

(add-to-list 'ac-dictionary-directories (get-vc-dir "auto-complete/dict"))

(ac-config-default)

(setq ac-quick-help-delay 0.2
      ac-use-comphist nil
      ac-quick-help-prefer-x t
      ac-auto-start nil)

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

(defadvice* point-stack-push before (anything-c-etags-default-action
                                     isearch-mode find-function find-library
                                     find-variable find-face-definition imenu
                                     fastnav-search-char-forward)
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

(ido-ubiquitous-disable-in magit-read-rev)

(dolist (mode '(ruby js2 js coffee html))
  (add-lambda (intern (concat (symbol-name mode) "-mode-hook"))
    (subword-mode 1)))

(defun fold-grep-command ()
  (if (> (length (match-string 1)) fill-column)
      (let ((beg (match-beginning 1))
            (end (match-end 1))
            (beg-show (/ (- fill-column 5) 2)))
        (let ((inhibit-read-only t))
          (put-text-property (+ beg beg-show) (- end (- fill-column beg-show 5))
                             'display " ... ")))))

(font-lock-add-keywords
 'grep-mode '(("^\n\\(grep .*$\\)"
               (1 (progn (fold-grep-command) 'bold)))))

(provide 'devenv)
