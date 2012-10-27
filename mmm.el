(ulp-site "mmm-mode" t)

(require 'mmm-auto)

(eval-after-load "mmm-vars"
  `(progn
     (mmm-add-mode-ext-class 'html-erb-mode nil 'html-js)
     (mmm-add-mode-ext-class 'html-erb-mode nil 'html-css)
     (mmm-add-mode-ext-class 'html-erb-mode "\\.html\\.erb\\'" 'erb)
     (mmm-add-mode-ext-class 'html-erb-mode "\\.jst\\.ejs\\'" 'ejs)
     (set-face-attribute 'mmm-code-submode-face nil :background "ghost white")
     (set-face-attribute 'mmm-output-submode-face nil :background "honeydew")))

(add-auto-mode 'html-erb-mode "\\.html\\.erb\\'" "\\.jst\\.ejs\\'")

(setq mmm-global-mode 'auto
      mmm-submode-decoration-level 2
      mmm-parse-when-idle t)

(defun html-erb-flyspell-predicate ()
  (not (eq 'tag (car (sgml-lexical-context)))))

(put 'html-erb-mode 'flyspell-mode-predicate 'html-erb-flyspell-predicate)

(provide 'mmm)
