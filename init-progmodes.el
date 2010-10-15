(update-load-path-vc "eproject")
(update-load-path-vc "emacs-nav" t)
(update-load-path "~/emacs-hs" t)

(require 'eproject)
(require 'eproject-extras)

(define-project-type make (generic) (look-for "Makefile"))
(define-project-type rake (generic) (look-for "Rakefile"))
(define-project-type lein (generic) (look-for "project.clj"))
(define-project-type gae (generic) (look-for "app.yaml"))
(define-project-type scons (generic) (look-for "SConstruct"))
(define-project-type ant (generic) (look-for "build.xml"))
(define-project-type haskell (generic) (look-for "Setup.hs"))
(define-project-type emacs (generic) (look-for "init.el"))

(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

(add-hook 'scons-project-file-visit-hook
          (lambda ()
            (setq-local compile-command (format "cd %s && scons" (eproject-root)))))

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local c-basic-offset 2)))

(autoload 'ghc-init "ghc" nil t)

(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)
            (turn-on-haskell-doc-mode)
            (turn-on-font-lock)
            (ghc-init)))

(setq *jdh-javadocs*
      (mapcar (lambda (url) `(,url nil t t))
              '("http://java.sun.com/javase/6/docs/api/"
                "http://java.sun.com/products/servlet/2.5/docs/servlet-2_5-mr2/"
                "http://commons.apache.org/codec/api-release/"
                "http://commons.apache.org/io/api-release/"
                "http://commons.apache.org/lang/api-release/")))

(defun javadocs-refresh (&optional force)
  (interactive "p")
  (if (or force
          (not (file-exists-p javadoc-help-cache-dir)))
      (mapc (lambda (javadoc)
              (jdh-refresh-url (car javadoc)))
            *jdh-javadocs*)))

(eval-after-load 'javadoc-help
  '(javadocs-refresh))

(provide 'init-progmodes)
