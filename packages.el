(add-to-list 'load-path user-emacs-directory)

(package-initialize)
(package-refresh-contents)

(defvar packages-list
  '(ace-link ace-window
    clojure-mode coffee-mode discover
    flymake-coffee flymake-ruby helm-descbinds
    highlight-escape-sequences highlight-tail ido-ubiquitous
    iflipb inf-ruby iy-go-to-char markdown-mode move-text multiple-cursors
    paredit rainbow-mode ruby-end smart-newline morlock
    smex switch-window typing undo-tree wgrep win-switch ruby-hash-syntax
    yaml-mode yasnippet bbdb whitespace-cleanup-mode))

(dolist (p packages-list)
  (unless (or (member p package-activated-list)
              (functionp p))
    (package-install p)))
