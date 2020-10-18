;;; ezkeys.el --- Easy syntax for keymaps of highest precedence.  -*- lexical-binding: t -*-

;;; Usage:
;;
;; Add the directory containing this file to your `load-path'
;;
;; Define a keymap with `ezk-defkeymaps'.


;;;; ===========================================================================
;;;;                                custom settings 

(defgroup ezk nil
  "ezk Keymap"
  :group 'ezkeys)

(defun ezk/lat? (form)
  "List of atoms?"
  (cond ((null form) t)
        ((listp (car form)) nil)
        (t (ezk/lat? (cdr form)))))

;;;; ===========================================================================
;;;;                    emulation mode map alist and global map 

(defvar ezk-mode-map-alist '())

;; to guarantee `ezk-mode-map-alist' is first on `emulation-mode-map-alists'
;; after loading so it has highest precedence
(setq emulation-mode-map-alists
      (cons 'ezk-mode-map-alist (remove 'ezk-mode-map-alist ;allow multiple loads
                                         emulation-mode-map-alists)))

(defvar _ezk/global-map (make-sparse-keymap))

(define-minor-mode _ezk/minor
  "ezk"
  :init-value t
  :lighter " ezk"
  :keymap _ezk/global-map)

(define-globalized-minor-mode _ezk/global
  _ezk/minor
  (lambda () (_ezk/minor 1)))                ;on in every buffer


;;;; ===========================================================================
;;;;                     create maps and modes and add to hooks 

(defun ezk/as-explicit-hook (hook)
  "Makes HOOK explicit by guaranteeing it ends in
\"-hook\". Unless, of course, HOOK is GLOBAL. In which case, the
symbol returned is just global"
  (if (eq hook 'GLOBAL)
      (intern "global")
    (let* ((hook-name (symbol-name hook))
           (is-explicit (string-match-p ".*?-hook" hook-name)))
      (if is-explicit
          hook
        (intern (concat hook-name "-hook"))))))

;; TODO: make it so this returns explicit hooks. Pay attention to interning
;; symbols. I didn't and screwed up when attempting the change.
(defun ezk/extract-hooks (map)
  "Extracts hooks from the map given to `ezk-defkeymaps'. Doesn't
include the GLOBAL symbol in its output. Order of hooks returned
is same as order given in map. Hooks returned may be explicit or
implicit."
  (cl-labels ((extract (form)
                       (cond ((symbolp form) nil)
                             ((ezk/lat? (cdr form)) (cdr form)) ; (f hook [hook]...)
                             (t (mapcan #'extract form)))))
    (delete-dups (remove 'GLOBAL (mapcan #'extract map)))))

(defun ezk/minor-mode-sym (hook)
  "Returns a symbol ezk uses to store the maps made for HOOKs."
  ;; preceded with _ because these really should be hidden
  (if (eq hook 'GLOBAL)
      '_ezk/global                      ;GLOBAL is special case
    (intern (format "_ezk/%s-minor-mode"
                    (symbol-name (ezk/as-explicit-hook hook))))))

(defun ezk/minor-mode-map-sym (minor-mode-name)
  "Name returned is based on defined behavior of
`define-minor-mode'."
  (intern (format "%s-map" (symbol-name minor-mode-name))))

(defun ezk/get-mode-map-name (hook)
  "Convenience function. Returns the keymap created when an ezk
minor mode was defined for HOOK"
  (ezk/minor-mode-map-sym (ezk/minor-mode-sym hook)))

(defun ezk/get-mode-map (hook)
  "Get the ezk mode map generated and added to HOOK."
  (eval (ezk/get-mode-map-name hook)))

(defun ezk/define-minor-mode (name)
  (eval `(define-minor-mode ,name
     "A minor mode defined by `ezkeys'. Purely for internal use."
     nil
     nil                    ;no lighter because it just duplicates information
     (make-sparse-keymap)   ;behavior of `define-minor-mode' is to setq NAME-map
     )))

(defun ezk/create-modes-and-maps (hooks)
  "Defines minor modes and maps for each hook."
    (mapc (lambda (hook)
            (let* ((mode-name (ezk/minor-mode-sym hook))
                   (map-name (ezk/minor-mode-map-sym mode-name)))
              (unless (assoc mode-name ezk-mode-map-alist)
                (ezk/define-minor-mode mode-name)
                ;; appends. so the sooner a hook, the higher its precedence
                (add-to-list 'ezk-mode-map-alist
                             `(,mode-name . ,(eval map-name))
                             t)
                (add-hook hook mode-name))))
          (mapcar #'ezk/as-explicit-hook hooks)))


;;;; ===========================================================================
;;;;                     resolve keymaps and add to their modes 

(defvar ezk/seen (obarray-make)
  "Stores key definitions seen while processing the keymap. Form
of symbols is HOOK.KEYS, e.g. c-mode-hook.C-xM-a<f10>")

(defun ezk/not-defined? (keys hook)
  "KEYS is a list of key symbols. t if the KEYS and HOOK
combination has never been given to this function. Otherwise
nil."
  (let* ((name (apply #'concat
                      (symbol-name (ezk/as-explicit-hook hook))
                      "."
                      (mapcar #'symbol-name keys)))
         (defined? (obarray-get ezk/seen name)))
    (obarray-put ezk/seen name)
    (not defined?)))

(defun ezk/kbd (keys)
  "Returns internal emacs representation of KEYS."
  (kbd (apply #'concat (mapcar (lambda (k) (concat (symbol-name k) " ")) ;add space
                               keys))))

(defun ezk/define-key (fun hooks keys)
  "HOOKS can be implicit or explicit. Makes it so KEYS in maps
created for HOOKS by `ezk/create-modes-and-maps' will call FUN."
  (mapc (lambda (hook)
          (let ((map (ezk/get-mode-map hook))
                (keys (ezk/kbd keys)))
            (define-key map keys fun)))
        hooks))

;; todo: refactor
(defun ezk/process-form (form keys)
  "TODO"
  (cond ((ezk/lat? (cdr form))          ;(f hook [hook]...)
         (ezk/define-key (car form) (cdr form) keys))
        (t
         (do ((curr (car form) (car rest))
              (newkeys '())
              (rest (cdr form) (cdr rest)))
             ((listp curr)
              (mapc (lambda (form)
                      (ezk/process-form form
                                        (concatenate 'list keys newkeys)))
                    (cons curr rest)))
           (setq newkeys (append newkeys `(,curr)))))))

(defun ezk/process-map (map)
  (mapc (lambda (form)
          (ezk/process-form form '()))
        map))

;;;; ===========================================================================
;;;;                                      main 

;;;###autoload
(defmacro ezk-defkeymaps (&rest map)
  "MAP is any number of forms like:
((KEY [KEY]...)... (FUN HOOK [HOOK]...)

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

(ezk-defkeymaps
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
"
  (declare (indent defun))
  `(progn
     (ezk/create-modes-and-maps (ezk/extract-hooks ',map))
     (ezk/process-map ',map)
     ;; ensure `_ezk/global' has lowest precedence
     (assoc-delete-all '_ezk/global ezk-mode-map-alist)
     (add-to-list 'ezk-mode-map-alist '(_ezk/global . ,_ezk/global-map) t)
     t))


(_ezk/global 1)                         ;enable global map when `require'ed
(provide 'ezkeys)

;;;; ===========================================================================
;;;;                                 example usage 

;; (ezk-defkeymaps
;;   (<f9> (C-x C-x C-h (hello GLOBAL))
;;         (C-x (C-g
;;               (goodbye GLOBAL))
;;              (C-b
;;               (something-between GLOBAL))))

;;   (<f11> <f10> <f10>
;;        ((lambda () (interactive) (message "this is a lisp C-l C-l C-l")) lisp-mode emacs-lisp-mode scheme-mode)
;;        ((lambda () (interactive) (message "this is either c or c++ C-l C-l C-l")) c++-mode c-mode)
;;        )
;;   )

;;;; ===========================================================================
;;;;                                      todo 


;; 1. Prevent user from redefining a KEYSEQ.MODE-function binding during same
;; epoch. A new epoch is created each time `ezk-defkeymaps' is execd. Just use
;; `gensym' for epoch.

;; 3. Allow user to set specific precedence levels on a hook by hook
;; basis. GLOBAL will always be lowest precedence, but `ezk-defkeymaps' will
;; take in a precedence plist. Like (c-mode 1 scheme-mode 2 some-other-mode
;; 99). The closer the number to 1, the higher the precedence. All modes not
;; definied in PRECEDENCE will be assumed to be lower.


;; what if to the GLOBAL symbol we added optional expressions like: (not HOOK)
;; (not HOOK2) ... meaning, this binding in GLOBAL but not HOOK nor HOOK2
;;
;; this should just involve permantely turning a map's mode on and only adding a
;; (mode -1) to the hooks of those in the NOT form.

;; TODO warn user about conflicting keys. ie, warn them when KEYSEQ has already
;; been defined for HOOK. We cannot guarrantee that there won't be conflicts
;; when KEYSEQ is defined for two different modes that may be simultaneously
;; active.

;; when you get to (K (f hook...)) you will have to know the preceding string of
;; keys. Each of these will be parsed `ezk/as-key'. And sent to ezk/construct-keyseq

