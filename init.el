(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'cl)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'assoc)

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)

(setq package-user-dir (concat dotfiles-dir "elpa")
      custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(require 'init-defuns)
(update-load-path (concat dotfiles-dir "site-lisp"))
(update-load-path-vc "emacs-starter-kit")
(update-load-path-vc "emacs-starter-kit/elpa-to-submit" t)

(require 'package)
(package-initialize)

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-eshell)
(require 'starter-kit-ruby)
(require 'starter-kit-js)

(remove-hook 'coding-hook 'idle-highlight)

(require 'init-progmodes)
(require 'init-slime)
(require 'init-completion)
(require 'init-misc)
(require 'init-keys)

(if (file-exists-p locals-file) (load locals-file))
(update-autoloads)
