;;; ezkey-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "ezkeys" "ezkeys.el" (0 0 0 0))
;;; Generated autoloads from ezkeys.el

(autoload 'ezk-defkeymaps "ezkeys" "\
MAP is any number of forms like:
\((KEY [KEY]...)... (FUN HOOK [HOOK]...)

KEY is a symbol similar to the strings provided to `kbd' (but not
a string) (e.g. C-x, <f10>, M-x, a, b, c, 1, 2, 3).

FUN is a symbol bound to some interactive function or a
lambda.

HOOK is a name bound to a list of functions to run when some mode
is turned on (e.g. c-mode-hook). HOOK may also just be a symbol
bound to a mode (e.g. c-mode). In this case, it's assumed there
exists a variable `c-mode-hook'. Or hook may be the special
symbol GLOBAL. Binding a key to a function on the GLOBAL hook is
similar to defining a key using `global-set-key'. The binding
will always be available unless some other binding bound to a
more specific hook exists.

===

Precedence of keymaps
TODO

===

e.g.
Where HELLO and GOODBYE are bound to interactive functions

\(ezk-defkeymaps
  (<f9> (C-x C-x C-h (hello GLOBAL))
        (C-x (C-g
              (goodbye GLOBAL)
             (C-g
              (hello lisp-mode))
             (C-g C-g
              (hello c-mode)))))

  (<f11> <f10> <f10>
       ((lambda () (interactive) (message \"this is a lisp\")) lisp-mode emacs-lisp-mode scheme-mode)
       ((lambda () (interactive) (message \"this is either c or c++\")) c++-mode c-mode))))

===
The above code produces the following behavior.

Anywhere:
<f9> C-x C-x C-h  => call hello

Anywhere besides lisp-mode and c-mode:
<f9> C-x C-g      => call goodbye

Only in lisp mode:
<f9> C-x C-g      => call hello
*Overrides the GLOBAL binding*

Only in c-mode:
<f9> C-x C-g C-g  => call hello
*While it doesn't exactly match the GLOBAL binding, it still
 overrides it, because the full keysequence has a prefix exactly
 matching the GLOBAL map's binding.*

In lisp-mode, emacs-lisp-mode or scheme-mode

<f11> <f10> <f10> => call a lambda printing \"this is a lisp\"

In c++-mode or c-mode

<f11> <f10> <f10> => call a lambda printing \"this is either c or c++\"
===

Note that in the example above, all of the hooks (lisp-mode,
emacs-lisp-mode, c-mode, etc) are really just symbols bound to
modes. This only works because all of these \"hooks\" actually
define a hook with the same name as their mode, but with
\"-hook\" appended (lisp-mode-hook, emacs-lisp-mode-hook,
etc.). While this behavior is typical of `define-minor-mode' and
convention for major modes, it's not guaranteed. For any hooks
that don't follow this convention, the exact hook needs to be
given.

\(fn &rest MAP)" nil t)

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
