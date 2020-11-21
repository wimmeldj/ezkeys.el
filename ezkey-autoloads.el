;;; ezkey-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ezkeys" "ezkeys.el" (0 0 0 0))
;;; Generated autoloads from ezkeys.el

(autoload 'ezk-defkeymaps "ezkeys" "\
Define keymaps allocated on `emulation-mode-map-alists'

MAP is any number of forms like:
\((KEY [KEY]...)... (DEF HOOK [HOOK]...)

KEY is a string like the strings provided to `kbd' (eg. \"C-x\"
\"<f10>\" etc.)

DEF is any DEF accepted by `define-key'. This is the action that
the key sequences perform.

HOOK is any ordinary hook, like `c-mode-hook' or
`lisp-mode-hook' (explicit). HOOK may also just be a symbol bound
to a mode - e.g. c-mode - (implicit). In this case, it's assumed
that the hook corresponding to `c-mode' has the standard name,
i.e. `c-mode-hook'.

Additionally, a hook may be the special symbol GLOBAL. Binding a
key to GLOBAL is similar to defining a key using
`global-set-key'. The binding will be available in all buffers
unless some other binding bound to a more specific hook
exists. In which case, the more specific binding overrides the
global binding in all buffers that run the hooks it's defined on.

===

GROUP-ALIST can be used to name a list of modes in a keymap
terminal form: (DEF HOOK [HOOK]...)

For instance, if GROUP-ALIST was

\((LISP lisp-mode scheme-mode emacs-lisp-mode-hook))

Any terminal form that uses LISP as a hook, will apply the keymap
definition to all of the \"hooks\" in the cdr of the alist
member. In this case,

\(DEF LISP)
= (DEF lisp-mode scheme-mode emacs-lisp-mode-hook)
= (DEF lisp-mode-hook scheme-mode-hook emacs-lisp-mode-hook)

You can use any number of these group aliases in a terminal form.

===

Hooks appearing earlier in PRECEDENCE-LIST have higher precedence
than those appearing later. So in the case that two modes are
active that bind the same key, the one appearing earlier in
PRECEDENCE-LIST will have its action taken. Any hook not included
in PRECEDENCE-LIST has a lower precedence than those
mentioned. This applies to all hooks except GLOBAL, which always
has the lowest precedence.

\(fn PRECEDENCE-LIST GROUP-ALIST &rest MAP)" nil t)

(function-put 'ezk-defkeymaps 'lisp-indent-function 'ezk/tl-indent)

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
