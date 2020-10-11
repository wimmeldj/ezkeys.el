;;; ezkey-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ezkeys" "ezkeys.el" (0 0 0 0))
;;; Generated autoloads from ezkeys.el

(autoload 'ezk-defkeymaps "ezkeys" "\
LIGHTERS like
\((c-mode-hook . \"C\")
\(scheme-mode-hook . \"λ\")
\(lisp-mode-hook . \"CL\")
\(emacs-lisp-mode-hook . \"EL\"))

MAP like
TODO

\(fn LIGHTERS &rest MAP)" nil t)

(function-put 'ezk-defkeymaps 'lisp-indent-function 'defun)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "ezkeys" '("_ezk/" "ezk")))

;;;***

(provide 'ezkey-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ezkey-autoloads.el ends here
