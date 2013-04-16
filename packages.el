(add-to-list 'load-path user-emacs-directory)

(require 'defuns)
(require 'prefs)

(package-initialize)
(package-refresh-contents)

(defvar packages-list
  '(autopair clojure-mode coffee-mode elisp-slime-nav
    expand-region flymake-coffee flymake-ruby helm-descbinds ido-ubiquitous
    iflipb inf-ruby iy-go-to-char markdown-mode move-text multiple-cursors
    paredit rainbow-mode ruby-electric rinari haml-mode sass-mode smex
    switch-window typing undo-tree wgrep win-switch
    yaml-mode yasnippet))

(dolist (p packages-list)
  (unless (or (member p package-activated-list)
              (functionp p))
    (package-install p)))
