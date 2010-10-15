(update-load-path "~/emacs-ac")
(update-load-path-vc "ac-slime")
(update-load-path-vc "pos-tip")

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

(provide 'init-completion)
