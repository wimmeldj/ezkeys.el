;;; ezkeys.el --- Easy syntax for keymaps of highest precedence.  -*- lexical-binding: t -*-

;;; Usage:
;;
;; Add the directory containing this file to your `load-path' and byte compile
;; optionally.
;;
;; tell ezkeys before you load it where your keymap definition will be. This needs to be a file in
;; your load path.
;; 
;; (setq ezk-keymap-path "some-file")
;;
;; Finally, load ezkeys
;; 
;; (require 'ezkeys)
;;
;; You may want to manually execute this before you define a keymap (ie. C-x e) because it sets the
;; proper indentation for a keymap definition in the file at `ezk-keymap-path'.
;;
;; Define a keymap with `ezk-defkeymaps'
;; 

(require 'cl-macs)


;;;; ===========================================================================
;;;;                                custom variables 

(defgroup ezkeys nil
  "EZ high-precedence declarative keymaps"
  :group 'convenience)

(defcustom ezk-keymap-path
  (concat user-emacs-directory "keymap")
  "The path where the keymap file should be stored."
  :group 'ezkeys
  :type 'file)



;;;; ===========================================================================
;;;;                    emulation mode map alist and global map 

;; (defvar _ezk/global-map (make-sparse-keymap))

(define-minor-mode ezk-minor-mode
  "ezk"
  :init-value t
  :lighter " ezk")

(define-globalized-minor-mode _ezk/global
  ezk-minor-mode
  (lambda () (ezk-minor-mode 1)))                 ;on in every buffer

;; GLOBAL always exists and will always be last
(defvar ezk-mode-map-alist `((_ezk/global . ,(make-sparse-keymap))))

;; FIXME. This doesn't work so well for guaranteeing `ezk-mode-map-alist' has
;; highest precedence on `emulation-mode-map-alists'
(setq emulation-mode-map-alists
      (cons 'ezk-mode-map-alist (remove 'ezk-mode-map-alist ;allow multiple loads
                                         emulation-mode-map-alists)))


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
        (and (or (symbolp def)          ;function symbol or definition
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

(defun ezk/get-mode-map (hook)
  "Get the map corresponding to the ezk mode created for HOOK"
  (alist-get (ezk/minor-mode-sym hook) ezk-mode-map-alist))

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
     "A minor mode defined by `ezkeys' for use not by you.")))

(defun ezk/create-modes-and-maps (hooks)
  "Defines minor modes and maps for each hook.

For any hook that is already represented on `ezk-mode-map-alist',
the keymap is wiped so that the latest call to `ezk-defkeymaps'
reflects the state of `ezk-mode-map-alist'."
  (cl-flet ((wipe-keymap (hook)
                         (setf (alist-get (ezk/minor-mode-sym hook) ezk-mode-map-alist)
                               (make-sparse-keymap))))
    (mapc (lambda (hook)
            (let* ((minor-mode-sym (ezk/minor-mode-sym hook)))
              (ezk/define-minor-mode minor-mode-sym)
              (add-to-list 'ezk-mode-map-alist (cons minor-mode-sym (make-sparse-keymap)))
              ;; the hook triggers the ezk-specific minor mode
              (add-hook hook minor-mode-sym)))
          (mapcar #'ezk/as-explicit-hook hooks))))



;;;; ===========================================================================
;;;;                     resolve keymaps and add to their modes 

(defvar ezk/epoch
  (let ((i 0))
    (lambda () (setq i (1+ i)))))

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
                 (define-key map  kbd-keys def))
               (try-redefine (map hook prev-epoch-defn)
                             (let ((prev-epoch (car prev-epoch-defn))
                                   (prev-action (cdr prev-epoch-defn)))
                               (if (= prev-epoch epoch)
                                   (display-warning :error
                                                    (format "Bad key definition. %S on %s already defined to call %s. Ignoring directive to call %s."
                                                            keys hook prev-action def))
                                 (define map hook)))))
      (mapc (lambda (hook)
              (let* ((map (ezk/get-mode-map hook))
                     (k (cons hook kbd-keys)) ;`ezk/seen' key
                     (found (alist-get k ezk/seen nil nil #'equal)))
                (if found
                    (try-redefine map hook found)
                  (define map hook))))
            hooks))))

(defun ezk/process-form (form keys epoch)
  "Used to process each member of the MAP given to
`ezk-defkeymaps'. Assumes that `ezk/create-modes-and-maps' has
already been called to allocate the correct modes and maps."
  (cond ((ezk/terminal? form)           ;(f hook [hook]...)
         (ezk/define-keys (car form) (cdr form) keys epoch))
        (t
         (cl-do ((curr (car form) (car rest))
              (newkeys '())
              (rest (cdr form) (cdr rest)))
             ((listp curr)
              (mapc (lambda (form)
                      (ezk/process-form form
                                        (append keys newkeys)
                                        epoch))
                    (cons curr rest)))
           (setq newkeys (append newkeys (list curr)))))))

(defun ezk/process-map (map)
  (let ((epoch (funcall ezk/epoch)))
    (mapc (lambda (form)
            (ezk/process-form form '() epoch))
          map)))



;;;; ===========================================================================
;;;;                               preprocess keymap 

(defun ezk/preprocess-map (group-alist map)
  "Symbols in MAP definitions (DEF HOOK [HOOK]...) `eq' to a car
in `group-alist' will be replaced by a spliced list specified in
the cdr. MAP is not modified, but a new map returned."
  (cl-labels ((repl (f)
                    (cond ((or (atom f) (stringp f)) f)
                          ((ezk/terminal? f) ;(DEF HOOK [HOOK]...)
                           (let (substitues)
                             (mapc (lambda (symb)
                                     (setq substitues
                                           (append substitues
                                                   (alist-get symb group-alist (list symb)
                                                              nil #'eq))))
                                   (cdr f))
                             `(,(car f) ,@substitues)))
                          (t (mapcar #'repl f)))))
    (mapcar #'repl map)))



;;;; ===========================================================================
;;;;                         arrange keymaps by precedence 

(defun ezk/order-maps (precedence)
  "Arranges `ezk-mode-map-alist' to match PRECEDENCE"
  (let* ((precedence (remove 'GLOBAL precedence)) ;no explicit precedence of GLOBAL allowed
         (reversed (reverse precedence))
         (ordered (copy-tree ezk-mode-map-alist)))
    (mapc (lambda (mode)
            (let ((map (alist-get mode ordered)))
              (when (assoc mode ordered)
                (setq ordered (assoc-delete-all mode ordered))
                (push (cons mode map) ordered))))
          (mapcar #'ezk/minor-mode-sym reversed))
    ordered))



;;;; ===========================================================================
;;;;                                      main 

;;;###autoload
(defmacro ezk-defkeymaps (precedence-list group-alist &rest map)
  "Define keymaps allocated on `emulation-mode-map-alists'

MAP is any number of forms like:
((KEY [KEY]...)... (DEF HOOK [HOOK]...)

KEY is a string like the strings provided to `kbd' (eg. \"C-x\"
\"<f10>\" etc.)

DEF is any DEF accepted by `define-key'. This is the action that
the key sequences perform.

HOOK is any ordinary hook, like `c-mode-hook' or
`lisp-mode-hook' (explicit). HOOK may also just be a symbol bound
to a mode - e.g. c-mode - (implicit). In this case, it's assumed
that the hook corresponding to `c-mode' has the standard name,
i.e. `c-mode-hook'. The hook need not be a variable currently
loaded when this macro is evaled. e.g. `python-mode-hook' may not
be available when emacs starts, but it's still okay to reference
`python-mode-hook' (explicit) or `python-mode' (implicit) when
calling this macro. When the code defining `python-mode-hook' is
eventually loaded, keymaps will work as expected.

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

((LISP lisp-mode scheme-mode emacs-lisp-mode-hook))

Any terminal form that uses LISP as a hook, will apply the keymap
definition to all of the \"hooks\" in the cdr of the alist
member. In this case,

(DEF LISP)
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
"
  (declare (indent ezk/tl-indent))
  `(progn
     (let ((newmap (ezk/preprocess-map ',group-alist ',map)))
       ;; wipe all previously defined maps to keep clean state
       (mapc (lambda (memb)
               (setf (cdr memb) (make-sparse-keymap)))
             ezk-mode-map-alist)

       ;; creates modes and maps and store on `ezk-mode-map-alist'
       (ezk/create-modes-and-maps (ezk/extract-hooks newmap))
       ;; parse the keymap and store it in `ezk-mode-map-alist'
       (ezk/process-map newmap)
       ;; reorder `ezk-mode-map-alist' to respect PRECEDENCE-LIST
       (setq ezk-mode-map-alist (ezk/order-maps ',precedence-list))

       ;; turn `_ezk/emacs-lisp-mode-hook-minor-mode' on if called in emacs-lisp
       ;; buffer. Otherwise, won't turn on till `emacs-lisp-mode-hook' is execed
       (when (eq major-mode 'emacs-lisp-mode)
         (let ((ezk-emacs-mode-sym (ezk/minor-mode-sym 'emacs-lisp-mode)))
           (if (assoc ezk-emacs-mode-sym ezk-mode-map-alist)
               (apply ezk-emacs-mode-sym 1 nil)
             (and (fboundp ezk-emacs-mode-sym)
                  (apply ezk-emacs-mode-sym -1 nil)))))
       t)))

;; little indentation at top level
(defun ezk/tl-indent (_ _) '(1))



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
            ;; indent to last balanced list at same depth. Applies to both MAP
            ;; and GROUP-ALIST. FIXME: should use default-lisp-indent
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
  (with-temp-buffer (write-file ezk-keymap-path))
  (write-region "\
;;;; -*- mode: emacs-lisp; -*-
;;;; EZKEYS Keymap File"
              nil
              ezk-keymap-path))

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

;; what if to the GLOBAL symbol we added optional expressions like: (not HOOK)
;; (not HOOK2) ... meaning, this binding in GLOBAL but not HOOK nor HOOK2
;;
;; this should just involve permantely turning a map's mode on and only adding a
;; (mode -1) to the hooks of those in the NOT form.

;; interactive definitions of keymaps. Execute command, type in key sequence,
;; tab inserts a list group. When finished, updates precedence if necessary.

;; eg
;; (ezk-insert-key)
;; Type key sequence. tab when done: C-x or C-x M-s, or <f10>, etc
;; <tab> => ("C-x" "M-s" (|))
;; type another key sequence or a definition. Tab when done.
;; :defn hook hook or group
;; <tab> => ("C-x" "M-s" (def hook hook group))
;; (ezk-update-precedence-list)
