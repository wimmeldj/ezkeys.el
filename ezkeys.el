;; -*- lexical-binding: t; -*-


;;;; ===========================================================================
;;;;                                custom settings 

(defgroup ezk nil
  "ezk Keymap"
  :group 'ezkeys)

(defcustom ezk-lighter-prefix " ["
  "The shared prefix for any lighter symbols loaded by ezk"
  :group 'ezkeys
  :type 'string)

(defcustom ezk-lighter-suffix "]"
  "The shared suffix for any lighter symbols loaded by ezk"
  :group 'ezkeys
  :type 'string)

(defcustom ezk-no-lighter nil
  "t if you don't want any ezk lighters displayed"
  :group 'ezkeys
  :type 'boolean)

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

(defun ezk/try-get-lighter (hook lighters)
  "HOOK should not be generated with `ezk/as-explicit-hook'.
Returns lighter if one is found for either an implicit or
explicit HOOK."
(or (alist-get hook lighters)
    (alist-get (ezk/as-explicit-hook hook)
               lighters)))

(defun ezk/minor-mode-lighter (hook lighter-middle)
  (cond (ezk-no-lighter nil)
        (lighter-middle (concat ezk-lighter-prefix lighter-middle ezk-lighter-suffix))
        (t ;else, guess lighter-middle to be the first substr before '-' in hook
         (let* ((hook-name (symbol-name hook))
                (i (or (string-match-p "-" hook-name)
                       (length hook-name)))
                (lighter-middle (substring hook-name 0 i)))
           (concat ezk-lighter-prefix lighter-middle ezk-lighter-suffix)))))

(defun ezk/define-minor-mode (name lighter)
  (eval `(define-minor-mode ,name
     "A minor mode defined by `ezkeys'. Purely for internal use."
     nil
     ,lighter
     (make-sparse-keymap)  ;behavior of `define-minor-mode' is to setq NAME-map
     )))

(defun ezk/create-modes-and-maps (lighters hooks)
  "Defines minor modes and maps for each hook. The minor modes
defined will be run when their respective hook runs. LIGHTERS is
an alist with each member's car as a hook symbol and each cadr as
a string to be flanked by `ezk-lighter-prefix' and
`ezk-lighter-suffix' when the hook runs. HOOKS is a list of
symbols specifying a hook that should be used to load the ezkeys
map generated for it. Any members in HOOKS not in LIGHTERS will
have a lighter guessed for them. Any members in HOOKS with nil as
their cadr in LIGHTERS won't display any lighter. Set
`ezk-no-lighter' to disable all lighters."
  (let ((lighters (mapcar (lambda (form) `(,(ezk/as-explicit-hook (car form)) . ,(cdr form)))
                          lighters)))
    (mapc (lambda (hook)
            (let* ((mode-name (ezk/minor-mode-sym hook))
                   (map-name (ezk/minor-mode-map-sym mode-name))
                   (lighter (ezk/minor-mode-lighter hook (ezk/try-get-lighter hook lighters))))
              (unless (assoc mode-name ezk-mode-map-alist)
                (ezk/define-minor-mode mode-name lighter)
                ;; appends. so the sooner a hook, the higher its precedence
                (add-to-list 'ezk-mode-map-alist
                             `(,mode-name . ,(eval map-name))
                             t)
                (add-hook hook mode-name))))
          (mapcar #'ezk/as-explicit-hook hooks))))


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
(defmacro ezk-defkeymaps (lighters &rest map)
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

Precedence of keymaps
TODO


e.g.
Where HELLO and GOODBYE are bound to interactive functions

(ezk-defkeymaps
  (<f9> (C-x C-x C-h (hello GLOBAL))
        (C-x (C-g
              (goodbye GLOBAL))))

  (<f11> <f10> <f10>
       ((lambda () (interactive) (message "this is a lisp C-l C-l C-l")) lisp-mode emacs-lisp-mode scheme-mode)
       ((lambda () (interactive) (message "this is either c or c++ C-l C-l C-l")) c++-mode c-mode))))


"
  (declare (indent defun))
  `(progn
     (ezk/create-modes-and-maps ',lighters (ezk/extract-hooks ',map))
     (ezk/process-map ',map)
     ;; ensure `_ezk/global' has lowest precedence
     (assoc-delete-all '_ezk/global ezk-mode-map-alist)
     (add-to-list 'ezk-mode-map-alist '(_ezk/global . ,_ezk/global-map) t)
     t))


;;;; ===========================================================================
;;;;                                 example usage 

;; (defun hello ()
;;   (interactive)
;;   (message "hello"))

;; (defun goodbye ()
;;   (interactive)
;;   (message "goodbye"))

;; (defun something-between ()
;;   (interactive)
;;   (message "how are you?"))

(ezk-defkeymaps
  ((c-mode . "C") (scheme-mode . "λ") (lisp-mode-hook . "CL") (emacs-lisp-mode-hook . "EL"))

  (<f9> (C-x C-x C-h (hello GLOBAL))
        (C-x (C-g
              (goodbye GLOBAL))
             (C-b
              (something-between GLOBAL))))

  (<f11> <f10> <f10>
       ((lambda () (interactive) (message "this is a lisp C-l C-l C-l")) lisp-mode emacs-lisp-mode scheme-mode)
       ((lambda () (interactive) (message "this is either c or c++ C-l C-l C-l")) c++-mode c-mode)
       )
  )



;;;; ===========================================================================
;;;;                                      todo 

;; 1. Prevent user from redefining a KEYSEQ.MODE-function binding during same
;; epoch. A new epoch is created each time `ezk-defkeymaps' is execd. Just use
;; `gensym' for epoch.

;; 2. Remove the lighter functionality. It doesn't serve any purpose.

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

(_ezk/global 1)                         ;enable global map when `require'ed

(provide 'ezkeys)
