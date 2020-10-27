;;; ezkey-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ezkeys" "ezkeys.el" (0 0 0 0))
;;; Generated autoloads from ezkeys.el

(autoload 'ezk-defkeymaps "ezkeys" "\
 MAP is any number of forms like:
\((KEY [KEY]...)... (DEF HOOK [HOOK]...)

KEY is a string like the strings provided to `kbd' (eg. \"C-x\"
\"<f10>\" etc.)

DEF is any DEF accepted by `define-key'. This is the action that
the key sequences perform.

HOOK is a name bound to a list of functions to run when some mode
is turned on (e.g. c-mode-hook). HOOK may also just be a symbol
bound to a mode (e.g. c-mode). In this case, it's assumed there
exists a variable `c-mode-hook'. Or hook may be the special
symbol GLOBAL. Binding a key to a function on the GLOBAL hook is
similar to defining a key using `global-set-key'. The binding
will be available in all buffers unless some other binding bound
to a more specific hook exists. In which case, the more specific
binding overrides the global binding in all buffers that run the
hooks it's defined on.

===

Precedence of keymaps is somewhat arbitrarily determined. GLOBAL
is guaranteed to be of lowest precedence because it's obviously
important for more specific keymaps to override global
functionality (like `global-set-key'). However, all other keymaps
currently have their precedence determined by the order in which
they first occur in MAP. eg.

MAP:
\(K1 (K2 (DEF A B C))
    (K3 (DEF B C D)))
\(K4 (DEF X))

The precedence of the maps above looks like this:

high         low
+----------------
A B C D X GLOBAL

Because this is the left to right order in which they
occur. GLOBAL is always defined.

TODO This might force you to define keys in an unusual way. Say
`prog-mode' should have lower precedence and `c-mode' and
`python-mode' should have higher. Since c-mode and python-mode
won't simultaneously be active, you don't care what their
precedence is relative to each other.

If c-mode and prog-mode should share a KEYS DEF combination, they
might be defined like this.

\(K K (c-mode prog-mode))

This is fine, but now you have to make sure the first definition
for python-mode is above this defintion. If it's below, you have

+---------------------------
c-mode prog-mode python-mode

This isn't great and user should be able to set explicit
precedence levels at the mode map level.

\(fn LEXICAL-ENV &rest MAP)" nil t)

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
