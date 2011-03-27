(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))

(add-to-list 'load-path dotfiles-dir)

(setq package-user-dir (concat dotfiles-dir "elpa")
      custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

(require 'defuns)
(update-load-path (concat dotfiles-dir "site-lisp"))
(update-load-path-vc "emacs-starter-kit")
(update-load-path-vc "emacs-starter-kit/elpa-to-submit" t)

(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'assoc)
(require 'saveplace)
(require 'winring)

(require 'starter-kit-defuns)
(require 'starter-kit-bindings)
(require 'starter-kit-misc)
(require 'starter-kit-eshell)
(require 'starter-kit-ruby)
(require 'starter-kit-js)

(require 'package)
(package-initialize)
(remove-hook 'coding-hook 'idle-highlight)

(require 'prefs)
(require 'keys)
(require 'progmodes)
(require 'devenv)

(if (file-exists-p locals-file) (load locals-file))
(update-autoloads)

(line-number-mode t)
(column-number-mode t)
(winner-mode t)
(cua-mode t)
(autopair-global-mode)
(winring-initialize)
(global-auto-revert-mode t)
