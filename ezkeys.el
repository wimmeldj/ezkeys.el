;;; ezkeys.el --- Easy syntax for keymaps of highest precedence.  -*- lexical-binding: t -*-

;;; Usage:
;;
;; Add the directory containing this file to your `load-path'
;;
;; Define a keymap with `ezk-defkeymaps'.


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
;;;;                                     common 

;; TODO refactor to ezk/terminal-form
(defun ezk/terminal? (form)
  "t if form is a terminal form (DEF HOOK [HOOK]...). nil
otherwise. 

A DEF is similar to the DEF in `define-key'. Except that DEF
cannot be nil. ie.

 a command (a Lisp function suitable for interactive calling),
 a string (treated as a keyboard macro),
 a keymap (to define a prefix key),
 a symbol (when the key is looked up, the symbol will stand for its
    function definition, which should at that time be one of the above,
    or another symbol whose function definition is used, etc.),
 a cons (STRING . DEFN), meaning that DEFN is the definition
    (DEFN should be a valid definition in its own right),
 or a cons (MAP . CHAR), meaning use definition of CHAR in keymap MAP,
 or an extended menu item definition.

"
  (cl-labels ((lat? (f)
                    (seq-reduce (lambda (a b) (and (symbolp a) (symbolp b)))
                                f t)))
    (let ((def (car form))
          (hooks (cdr form)))
      (and (or (functionp def)          ;function symbol or definition
               (stringp def)            ;keyboard macro
               (consp def))             ;a number of things
           (not (null hooks))           ;at least one hook
           (lat? hooks)))))

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

(defun ezk/kbd (keys)
  "Returns internal emacs representation of KEYS."
  (kbd (apply #'concat (mapcar (lambda (k) (concat (symbol-name k) " ")) ;add space
                          keys))))


;;;; ===========================================================================
;;;;                     create maps and modes and add to hooks 

(defun ezk/extract-hooks (map)
  "Extracts hooks from the map given to `ezk-defkeymaps'. Doesn't
include the GLOBAL symbol in its output. Order of hooks returned
is same as order given in map. Hooks returned may be explicit or
implicit."
  (let ((map (copy-tree map)))          ;mapcan modifies SEQUENCE
    (cl-labels ((extract (f)
                         (cond ((symbolp f) nil)
                               ((ezk/terminal? f) (cdr f)) ; (f hook [hook]...)
                               (t (mapcan #'extract f)))))
      (delete-dups (remove 'GLOBAL (mapcan #'extract map))))))


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

(defvar ezk/epoch
  (let ((i 0))
    (lambda () (incf i))))

(defvar ezk/seen '()
  "An alist where each member is of form
((HOOK KEYS) . (EPOCH . FUNCTION))")

(defun ezk/define-keys (def hooks keys epoch)
  "HOOKS need not be explicit. KEYS on modes which execute one or
more HOOKS call DEF. If KEYS are redefined for a given HOOK in
the same EPOCH, warns and doesn't redefine, but continues
processing the remainder of the HOOKS."
  (let ((kbd-keys (ezk/kbd keys)))      ;internal key representation
    (cl-flet* ((define (map hook)
                 (setf (alist-get (cons hook keys) ezk/seen nil nil #'equal)
                       (cons epoch def))
                 (define-key map kbd-keys def))
               (try-redefine (map hook prev-defn)
                             (let ((prev-epoch (car prev-defn))
                                   (prev-fun (cdr prev-defn)))
                               (if (= prev-epoch epoch)
                                   (display-warning :error
                                                    (format "Bad key definition. %S on %s already definied to call %s. Ignoring directive to call %s."
                                                            keys hook prev-fun def))
                                 (define map hook))))
               (define-key (hook)
                 (let* ((map (ezk/get-mode-map hook))
                        (k (cons hook keys)) ;`ezk/seen' key
                        (prev-defn (alist-get k ezk/seen nil nil #'equal)))
                   (if prev-defn
                       (try-redefine map hook prev-defn)
                     (define map hook)))))
      (mapc #'define-key hooks))))

(defun ezk/process-form (form keys epoch)
  "Used to process each member of the MAP given to
`ezk-defkeymaps'. Assumes that `ezk/create-modes-and-maps' has
already been called to allocate the correct modes and maps."
  (cond ((ezk/terminal? form)           ;(f hook [hook]...)
         (ezk/define-keys (car form) (cdr form) keys epoch))
        (t
         (do ((curr (car form) (car rest))
              (newkeys '())
              (rest (cdr form) (cdr rest)))
             ((listp curr)
              (mapc (lambda (form)
                      (ezk/process-form form
                                        (concatenate 'list keys newkeys)
                                        epoch))
                    (cons curr rest)))
           (setq newkeys (append newkeys `(,curr)))))))

(defun ezk/process-map (map)
  (let ((epoch (funcall ezk/epoch)))
    (mapc (lambda (form)
            (ezk/process-form form '() epoch))
          map)))

;;;; ===========================================================================
;;;;                               preprocess keymap 

(defun ezk/with-sym-replacement (env map)
  "ENV is an alist where each car is a symbol and each cdr is a
symbol that can be interpreted by `kbd' when converted to a
string. This replaces all the symbols in MAP matching cars of env
with cdrs. e.g. ENV could be ((BS '\\) (DOT '\\.)) etc. so you
don't have to escape every special symbol with a \\ "
  (cl-labels ((repl (f)
                    (cond ((symbolp f)
                           (alist-get f env
                                      f nil #'eq))
                          ((ezk/terminal? f) ;(DEF HOOK [HOOK]...)
                           (cons (if (symbolp (car f))
                                     (alist-get (car f) env
                                                (car f) nil #'eq)
                                   (car f))                   ;handle DEF
                                 (mapcar #'repl (cdr f)))) ;handle hooks
                          (t (mapcar #'repl f)))))
    (repl map)))



;;;; ===========================================================================
;;;;                                      main 

;;;###autoload
(defmacro ezk-defkeymaps (lexical-env &rest map)
  "LEXICAL-ENV is an alist where each member's car is a symbol
and each cdr is an arbitrary sexp. This can be used to bind
symbols in MAP to values. You may for instance want to use it
like so:

(
(BS . \\\\) (QUI . \\?) (TIC . \\#) (DOT . \\.) (Q . \\')
(BQ . \\`) (PO . \\() (PC . \\)) (SC . \\;)
(fun . (lambda () (interactive) (message \"hello\")))
)

Here, symbols that are required to be escaped (e.g. \\, ?, #, ',
`, etc.) are aliased so that a backslash in a key sequence
defintion doesn't confuse you. A lambda function is also aliased
to fun, which might be useful it occurs multiple times in MAP.

===

MAP is any number of forms like:
((KEY [KEY]...)... (DEF HOOK [HOOK]...)

KEY is a symbol similar to the strings provided to `kbd' (but not
a string) (e.g. C-x, <f10>, M-x, a, b, c, 1, 2, 3).

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
  `(let ((newmap (ezk/with-sym-replacement ',lexical-env ',map)))
     (ezk/create-modes-and-maps (ezk/extract-hooks newmap))
     (ezk/process-map newmap)

     ;; ensure `_ezk/global' has lowest precedence
     (assoc-delete-all '_ezk/global ezk-mode-map-alist)
     (add-to-list 'ezk-mode-map-alist '(_ezk/global . ,_ezk/global-map) t)
     t))



(_ezk/global 1)                         ;enable global map when `require'ed
(provide 'ezkeys)



;;;; ===========================================================================
;;;;                                 example usage 


;; (ezk-defkeymaps
;;   ((BS . \\) (QUI . \?) (TIC . \#) (DOT . \.) (Q . \') (BQ . \`) (PO . \()
;;    (PC . \)) (SC . \;)

;;    (CL . lisp-mode) (EL . emacs-lisp-mode-hook)

;;    (fun . (lambda () (interactive) (message \"hello\"))))

  
;;   (<f11> <f10> <f10>
;;        ((lambda () (interactive) (message "+++this is a lisp")) lisp-mode EL scheme-mode)
;;        ((lambda () (interactive) (message "+++this is either c or c++")) c++-mode c-mode))
;;   (<f11> <f10>
;;          (SC ((lambda () (interactive) (message "semicolon ; !")) scheme-mode CL))
;;          (QUI ((lambda () (interactive) (message "question ?")) scheme-mode-hook CL))
;;          (TIC ((lambda () (interactive) (message "tictac #")) scheme-mode-hook CL))
;;          (BS ((lambda () (interactive) (message "baskslash \\")) scheme-mode-hook CL))
;;          (DOT ((lambda () (interactive) (message "dot .")) scheme-mode-hook CL))
;;          (Q ((lambda () (interactive) (message "quote '")) scheme-mode-hook CL))
;;          (BQ ((lambda () (interactive) (message "backquote `")) scheme-mode-hook CL))
;;          (PO ((lambda () (interactive) (message "left paren (")) scheme-mode-hook CL))
;;          (PC ((lambda () (interactive) (message "right paren )")) scheme-mode-hook CL))
;;          )
;;   ;; bad defn. python good but scheme not because already definied
;;   ;; (<f11> <f10> <f10>
;;   ;;        ((lambda () (interactive) (message "override")) python-mode scheme-mode))

;;   )



;;;; ===========================================================================
;;;;                                      todo 


;; emulation layers. Each layer is a mode on `emulation-mode-map-alist'. You can
;; scroll through layers. Only one layer is active at a time, otherwise would be
;; too confusing. If an emulation map has multiple layers, you woult want to
;; actually display a lighter for the currently active mode/layer

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
