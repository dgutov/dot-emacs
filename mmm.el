(update-load-path-vc "mmm-mode")

(require 'mmm-auto)

(eval-after-load "mmm-vars"
  `(progn
     (mmm-add-group
      'html-js
      '((js-script-cdata
         :submode js-mode
         :face mmm-code-submode-face
         :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
         :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>")
        (js-script
         :submode js-mode
         :face mmm-code-submode-face
         :front "<script[^>]*>[ \t]*\n?"
         :back "[ \t]*</script>"
         :insert ((?j js-tag nil @ "<script type=\"text/javascript\">\n"
                      @ "" _ "" @ "\n</script>" @)))))
     (mmm-add-group
      'html-css
      '((css-cdata
         :submode css-mode
         :face mmm-code-submode-face
         :front "<style[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
         :back "[ \t]*\\(//\\)?]]>[ \t\n]*</style>")
        (css
         :submode css-mode
         :face mmm-code-submode-face
         :front "<style[^>]*>[ \t]*\n?"
         :back "[ \t]*</style>"
         :insert ((?c css-tag nil @ "<style type=\"text/css\">\n"
                      @ "" _ "" @ "\n</style>" @)))))
     (mmm-add-classes
      '((eruby :submode ruby-mode :front "<%[#=]?" :back "-?%>"
               :match-face (("<%#" . mmm-comment-submode-face)
                            ("<%=" . mmm-output-submode-face)
                            ("<%" . mmm-code-submode-face))
               :insert ((?% erb-code nil @ "<%" @ " " _ " " @ "%>" @)
                        (?# erb-comment nil @ "<%#" @ " " _ " " @ "%>" @)
                        (?= erb-expression nil @ "<%=" @ " " _ " " @ "%>" @)))
        (etanni :submode ruby-mode :front "<\\?r\\|#{" :back "\\?>\\|}"
                :match-face (("<\\?r" . mmm-code-submode-face)
                             ("#{" . mmm-output-submode-face))
                :insert ((?r etanni-code nil @ "<?r" @ " " _ " " @ "?>" @)
                         (?{ etanni-expression nil @ "#{" @ "" _ "" @ "}" @)))))
     (dolist (mode (list 'html-mode 'nxml-mode))
       (mmm-add-mode-ext-class mode "\\.\\(r\\|x\\)?html\\(\\.erb\\)?$" 'html-js)
       (mmm-add-mode-ext-class mode "\\.\\(r\\|x\\)?html\\(\\.erb\\)?$" 'html-css)
       (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?$" 'eruby)
       (mmm-add-mode-ext-class mode "\\.xhtml$" 'etanni))
     (set-face-attribute 'mmm-code-submode-face nil
                         :background "ghost white")
     (set-face-attribute 'mmm-output-submode-face nil
                         :background "honeydew")))

(add-auto-mode 'html-mode "\.rhtml$" "\.html\.erb$" "\.xhtml$")

(provide 'mmm)
