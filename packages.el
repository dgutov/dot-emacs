(add-to-list 'load-path user-emacs-directory)

(require 'defuns)
(require 'prefs)

(package-initialize)
(package-refresh-contents)

(defvar packages-list
  '(auto-complete autopair clojure-mode coffee-mode elisp-slime-nav
    expand-region flymake-coffee helm-descbinds ido-ubiquitous
    iedit inf-ruby iy-go-to-char markdown-mode move-text paredit
    rainbow-mode ruby-electric ruby-tools sass-mode smex
    starter-kit starter-kit-bindings starter-kit-lisp
    switch-window typing undo-tree wgrep win-switch
    yaml-mode yasnippet))

(dolist (p packages-list)
  (unless (or (member p package-activated-list)
              (functionp p))
    (package-install p)))
