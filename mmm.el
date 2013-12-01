(ulp-site "mmm-mode" t)

(require 'mmm-defaults)

(eval-after-load "mmm-vars"
  `(progn
     (set-face-attribute 'mmm-code-submode-face nil :background "ghost white")
     (set-face-attribute 'mmm-output-submode-face nil :background "honeydew")))

(setq mmm-global-mode 'auto
      mmm-submode-decoration-level 2
      mmm-parse-when-idle t)

(defun html-erb-flyspell-predicate ()
  (not (eq 'tag (car (sgml-lexical-context)))))

(put 'html-erb-mode 'flyspell-mode-predicate 'html-erb-flyspell-predicate)

(provide 'mmm)
