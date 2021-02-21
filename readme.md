A simple syntax for defining key bindings with high precedence. This
is achieved by allocating the mode maps on
`emulation-mode-map-alists`.

`Groups` defines sets of modes for which a key binding should apply.

`PRECEDENCE` sets relative precedence for modes. All modes mentioned
in `PRECEDENCE` have a precedence higher than those not. All modes
mentioned after a given mode in `PRECEDENCE` have a lower precedence
than those before. Any modes not mentioned in `PRECEDENCE` have an
arbitrary precedence. This is only important for modes that may be
simultaneously active (e.g. a major mode like `c-mode` and a minor
mode like `company-mode`).

```emacs-lisp
(ezk-defkeymaps
 ;; precedence
 (company-mode
  c-mode)
 ;; groups
 ((G GLOBAL)
  (CC c-mode c++-mode)
  (LISP emacs-lisp-mode scheme-mode-hook lisp-mode))

 ;; map
 ("M-<f12>" (d-load-next-theme G))

 ("M-u" (universal-argument G))
 ("M-x" (counsel-M-x G))

 ("C-s" (counsel-grep-or-swiper G))

 ("C-x"
    ("o" (ace-window G))
    ("C-b" (ibuffer G))
    ("C-f" (counsel-find-file G))
    ("u" (undo-tree-visualize G))
    ("b" (ivy-switch-buffer G))
    ("r i" (counsel-register G))
    )

 ("C-c"
    ("C-r" (ivy-resume G))
    )

 ("C-h"
    ("v" (counsel-describe-variable G))
    ("f" (counsel-describe-function G))
    ("l" (counsel-find-library G))
    ("S" (counsel-info-lookup-symbol G))
    )

 ("C-;"
    ("m"
       ("m" (magit-status G))
       ("f" (magit-find-file G))
       ("c" (magit-file-checkout G))
       ("l" (magit-log-buffer-file G)))
    ("a" (avy-goto-line G))
    ("C-f" (fzf G))
    ("C-/" (company-files G))
    ("C-s" (counsel-ag G))
    ("u" (browse-url G))

    ("C-h" (man-follow CC))

    ("C-d" (d-dired-dotfiles-toggle dired-mode))
    )


 ("C-w" ("C-h" (winner-undo G))
        ("C-l" (winner-redo G)))

 )
 ```