(update-load-path "~/emacs-ac")
(update-load-path-vc "ac-slime")
(update-load-path-vc "pos-tip")
(update-load-path-vc "anything-config")
(update-load-path-vc "anything-config/extensions")

(require 'auto-complete-config)
(require 'ac-slime)
(require 'pos-tip)

(add-to-list 'ac-dictionary-directories "~/emacs-ac/ac-dict")

(ac-config-default)

(setq ac-quick-help-delay 0.2)
(setq ac-use-comphist nil)
(setq ac-quick-help-prefer-x t)
(setq ac-auto-start nil)

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
    (let ((dir (if eproject-mode eproject-root (anything-c-current-directory))))
      (mapcar (lambda (file)
                  `(,(file-relative-name file dir) . ,file))
              (if eproject-mode
                  (eproject-list-project-files)
                (directory-files dir nil "^[^._]"))))))

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
  (anything '(anything-c-source-buffers
              anything-c-project-files
              anything-c-source-recentf)))

(eval-after-load 'anything-config
  '(progn
     (define-key anything-map (kbd "C-z") nil) ; hide from persistent help
     (define-key anything-map (kbd "C-;") 'anything-execute-persistent-action)))

(provide 'init-completion)
