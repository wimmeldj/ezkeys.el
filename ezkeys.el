;;; ezkeys.el --- Easy syntax for keymaps of highest precedence.  -*- lexical-binding: t -*-

;;; Usage:
;;
;; Add the directory containing this file to your `load-path' and byte compile
;; optionally.
;;
;; (require 'ezkeys)
;;
;; You can edit your keymap by editing the contents file at `ezk-keymap-path'
;;



;;;; ===========================================================================
;;;;                                custom variables 

(defcustom ezk-keymap-path
  (concat user-emacs-directory "keymap.el")
  "The path where the keymap file should be stored.
Can be a file that already exists, nothing will be
overwritten. Cannot be an `org-mode' file."
  :group 'ezkeys
  :type 'file)



;;;; ===========================================================================
;;;;                    emulation mode map alist and global map 

(defvar ezk-mode-map-alist '())

;; FIXME. This doesn't work so well for guaranteeing `ezk-mode-map-alist' has
;; highest precedence on `emulation-mode-map-alists'
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
  (cl-labels ((lat? (hooks)
                    (seq-reduce (lambda (a b) (and (symbolp a) (symbolp b)))
                                hooks
                                t)))
    (when (listp form)
      (let ((def (car form))
            (hooks (cdr form)))
        (and (or (functionp def)          ;function symbol or definition
                 (stringp def)            ;keyboard macro
                 (consp def))             ;a number of things
             (not (null hooks))           ;at least one hook
             (lat? hooks))))))

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
  "Get the actual map generated and added to HOOK."
  (eval (ezk/get-mode-map-name hook)))

(defun ezk/kbd (keys)
  "Returns internal emacs representation of KEYS."
  (kbd (mapconcat #'identity keys " ")))



;;;; ===========================================================================
;;;;                     create maps and modes and add to hooks 

(defun ezk/extract-hooks (map)
  "Extracts hooks from the map given to `ezk-defkeymaps'. Doesn't
include the GLOBAL symbol in its output. Order of hooks returned
is same as order given in map. Hooks returned may be explicit or
implicit."
  (let ((map (copy-tree map)))          ;`mapcan' modifies SEQUENCE
    (cl-labels ((extract (f)
                         (cond ((stringp f) nil)
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
((HOOK KEYS) . (EPOCH . DEF))")

(defun ezk/define-keys (def hooks keys epoch)
  "HOOKS need not be explicit. KEYS on modes which execute one or
more HOOKS call DEF. If KEYS are redefined for a given HOOK in
the same EPOCH, warns and doesn't redefine, but continues
processing the remainder of the HOOKS."
  (let ((kbd-keys (ezk/kbd keys)))      ;internal key representation
    (cl-flet* ((define (map hook)
                 ;; overwrites previous definition (guaranteed in different epoch)
                 (setf (alist-get (cons hook kbd-keys) ezk/seen nil nil #'equal)
                       (cons epoch def))
                 (define-key map kbd-keys def))
               (try-redefine (map hook prev-epoch-defn)
                             (let ((prev-epoch (car prev-epoch-defn))
                                   (prev-action (cdr prev-epoch-defn)))
                               (if (= prev-epoch epoch)
                                   (display-warning :error
                                                    (format "Bad key definition. %S on %s already definied to call %s. Ignoring directive to call %s."
                                                            keys hook prev-action def))
                                 (define map hook))))
               (define-key (hook)
                 (let* ((map (ezk/get-mode-map hook))
                        (k (cons hook kbd-keys)) ;`ezk/seen' key
                        (found (alist-get k ezk/seen nil nil #'equal)))
                   (if found
                       (try-redefine map hook found)
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
                                        ;; (concatenate 'list keys newkeys)
                                        (append keys newkeys)
                                        epoch))
                    (cons curr rest)))
           ;; (setq newkeys (append newkeys `(,curr)))))))
           (setq newkeys (append newkeys (list curr)))))))

(defun ezk/process-map (map)
  (let ((epoch (funcall ezk/epoch)))
    (mapc (lambda (form)
            (ezk/process-form form '() epoch))
          map)))



;;;; ===========================================================================
;;;;                               preprocess keymap 

;; (defun ezk/with-sym-replacement (env map)
;;   "ENV is an alist where each car is a symbol and each cdr is a
;; symbol that can be interpreted by `kbd' when converted to a
;; string. This replaces all the symbols in MAP matching cars of env
;; with cdrs. e.g. ENV could be ((BS '\\) (DOT '\\.)) etc. so you
;; don't have to escape every special symbol with a \\ "
;;   (cl-labels ((repl (f)
;;                     (cond ((symbolp f)
;;                            (alist-get f env
;;                                       f nil #'eq))
;;                           ((ezk/terminal? f) ;(DEF HOOK [HOOK]...)
;;                            (cons (if (symbolp (car f))
;;                                      (alist-get (car f) env
;;                                                 (car f) nil #'eq)
;;                                    (car f))                ;handle DEF
;;                                  (mapcar #'repl (cdr f)))) ;handle hooks
;;                           (t (mapcar #'repl f)))))
;;     (repl map)))



;;;; ===========================================================================
;;;;                         arrange keymaps by precedence 

;; todo
(defun ezk/order-maps (precedence)
  "Arranges `ezk-mode-map-alist' to match PRECEDENCE"
  ;; ensure `_ezk/global' has lowest precedence
  (assoc-delete-all '_ezk/global ezk-mode-map-alist)
  (add-to-list 'ezk-mode-map-alist
               (cons '_ezk/global _ezk/global-map)
               t))



;;;; ===========================================================================
;;;;                                      main 

;;;###autoload
(defmacro ezk-defkeymaps (precedence-list substitution-alist &rest map)
  "Define keymaps allocated on `emulation-mode-map-alists'

MAP is any number of forms like:
((KEY [KEY]...)... (DEF HOOK [HOOK]...)

KEY is a string like the strings provided to `kbd' (eg. \"C-x\"
\"<f10>\" etc.)

DEF is any DEF accepted by `define-key'. This is the action that
the key sequences perform.

HOOK is any ordinary hook, like `c-mode-hook' or
`lisp-mode-hook'. HOOK may also just be a symbol bound to a
mode (e.g. c-mode). In this case, it's assumed there exists a
variable `c-mode-hook'. Or hook may be the special symbol
GLOBAL. Binding a key to GLOBAL is similar to defining a key
using `global-set-key'. The binding will be available in all
buffers unless some other binding bound to a more specific hook
exists. In which case, the more specific binding overrides the
global binding in all buffers that run the hooks it's defined on.

===

Precedence of keymaps is somewhat arbitrarily determined. GLOBAL
is guaranteed to be of lowest precedence because it's obviously
important for more specific keymaps to override global
functionality (like `global-set-key'). However, all other keymaps
currently have their precedence determined by the order in which
they first occur in MAP. eg.

MAP:
(K1 (K2 (DEF A B C))
    (K3 (DEF B C D)))
(K4 (DEF X))

The precedence of the maps above looks like this:

high         low
+----------------
A B C D X GLOBAL

Because this is the left to right order in which they
occur. GLOBAL is always defined.
"
  (declare (indent ezk/tl-indent))
  `(progn
     (ezk/create-modes-and-maps (ezk/extract-hooks ',map))
     (ezk/process-map ',map)
     (ezk/order-maps ',precedence-list)
     t))


;; no indentation at top level
(defun ezk/tl-indent (_ _) '(0))



;;;; ===========================================================================
;;;;                                  indentation 

;; so that `ezk-lisp-indent-function' may continue with normal indentation
(defvar ezk/prev-lisp-indent-function lisp-indent-function)

(defun ezk-lisp-indent-function (pos state)
  "Locally overrides `lisp-indent-function' in keymap file.
The only keymap file is at `ezk-keymap-path'."
  (let* ((begin (car (elt state 9)))
         (depth (car (save-excursion
                       (parse-partial-sexp begin pos))))
         (curr-list-start (elt state 1))
         (in-ezk-defkeymaps? (when begin
                               (save-excursion
                                 (goto-char begin)
                                 (looking-at "(ezk-defkeymaps")))))
    (catch 'ret
      (cl-flet ((default-lisp-indent ()
                  (funcall ezk/prev-lisp-indent-function pos state))
                (tl-indent ()
                           (funcall (get 'ezk-defkeymaps 'lisp-indent-function)
                                    nil nil)))
        (when (not in-ezk-defkeymaps?)
          (throw 'ret (default-lisp-indent)))
        (when (and in-ezk-defkeymaps? (= depth 1))
          (throw 'ret (tl-indent)))
        (cl-flet* ((in-first-param? ()
                                    (save-excursion
                                      (goto-char curr-list-start)
                                      (backward-sexp)
                                      (= begin (1- (point)))))
                   (looking-at-symbol? ()
                                       (looking-at "\\sw\\|\\s_"))
                   (in-terminal-form? ()
                                      (save-excursion
                                        (goto-char curr-list-start)
                                        (forward-char) ;skip over (
                                        (forward-sexp)
                                        (backward-sexp)
                                        (and (looking-at-symbol?)
                                             (not (equal (1- (point))
                                                         begin))
                                             (not (in-first-param?))))))
          (when (in-first-param?)
            (throw 'ret (default-lisp-indent)))
          (when (in-terminal-form?)
            (throw 'ret (default-lisp-indent)))
          (let ((last-balanced-list-pos (condition-case nil
                                            (scan-lists pos -1 0)
                                          (t nil))))
            ;; indent to last balanced list at same depth
            ;;; ("C-x" "C-y" (def h h)        ("C-x" ("C-y" (def h h))
            ;;; .............here             .......here
            (when last-balanced-list-pos
              (throw 'ret
                     (save-excursion
                       (goto-char last-balanced-list-pos)
                       (current-column)))))
            ;; align `lisp-body-indent' ahead of last left paren or 1+
            ;; `lisp-body-indent' ahead of last sexp
            ;;;                          ("C-x"
            ;;; ("C-x" "C-y"             ..("C-x"
            ;;; .......|..here           ..|..here
            (let ((offset (save-excursion
                            (goto-char pos)
                            (backward-sexp)
                            (if (looking-back "(" (1- (point)))
                                (current-column)
                              (1+ (current-column))))))
              (throw 'ret
                     (+ offset lisp-body-indent))))))))



;;;; ===========================================================================
;;;;                              side effects on load 

;; Defining a specific file to contain the map seems to be the only way to
;; locally override `lisp-indent-function' - outside of the complexity of
;; implementing something like MMM-mode, where we could have context sensitive
;; indentaion /within/ files.
(unless (file-exists-p ezk-keymap-path)
  (f-touch ezk-keymap-path))
(load ezk-keymap-path t)

(add-hook 'find-file-hook #'ezk/on-keymap-file-load)
(defun ezk/on-keymap-file-load ()
    (when (equal (expand-file-name ezk-keymap-path)
                 (expand-file-name (buffer-file-name)))
      ;; value of `lisp-indent-function' is funcalled by
      ;; `calculate-lisp-indent'. Locally set so that any effects on performance
      ;; will only be felt in `ezk-keymap-path'
      (setq-local lisp-indent-function 'ezk-lisp-indent-function)))



(_ezk/global 1)
(provide 'ezkeys)






;;;; ===========================================================================
;;;;                                      todo 

;; emulation layers. Each layer is a mode on `emulation-mode-map-alist'. You can
;; scroll through layers. Only one layer is active at a time, otherwise would be
;; too confusing. If an emulation map has multiple layers, you woult want to
;; actually display a lighter for the currently active mode/layer. This could be
;; used to write an evil-lite minor mode

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
