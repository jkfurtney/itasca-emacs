## Emacs modes for editing Itasca software data files.

FLAC FLAC3D UDEC 3DEC PFC www.itascacg.com/software

If you are a vim user see: https://github.com/juanpabloaj/vim-fish

### Installation:

Copy this file somewhere on the emacs load-path and
add `(require "itasca.el")` to your .emacs file.

### About

This package defines five emacs major modes for editing Itasca
software data files. The focus is on making FISH programming easier.
Code specific keyword and FISH intrinsic highlighting is provided
along with indenting and code navigation support.

| file extension(s) | mode |
| --------------    | ---- |
| .dat .fis .fin    | `itasca-general-mode` |
| .fdat             | `itasca-flac-mode` |
| .f3dat            | `itasca-flac3d-mode` |
| .udat             | `itasca-udec-mode` |
| .p3dat .p2dat     | `itasca-pfc-mode` |

`itasca-general-mode` does not have any code-specific keyword/FISH
highlighting. To associate a specific file extension with a specific mode (for example to open all .dat files in `itasca-flac-mode`) use:

    (add-to-list 'auto-mode-alist '("\\.dat$'" . itasca-flac-mode))

To set the mode on a per-file basis: put a comment in the following
form at the top of the file.

    ;; -*- mode: itasca-general -*-
    ;; -*- mode: itasca-flac -*-
    ;; -*- mode: itasca-flac3d -*-
    ;; -*- mode: itasca-pfc -*-
    ;; -*- mode: itasca-udec -*-

### Code navigation

These modes provide support for navigation by

* `beginning-of-defun` `C-M-a` Jump to the beginning of the current
FISH function.

* `end-of-defun` `C-M-e` Jump to the end of the current FISH function.

* `imenu` Support for jumping to FISH function definitions via an interactive menu.

### Indentation

Tab indents a line of FISH code to the appropriate depth.
`indent-region` `C-M-\` also works.

### Yasnippet expansions

A set of templates for common FISH programming structures is provided.
This requires the `yasnippet` package. Copy the snippets to a location
where yasnippet can find them or add something like this to your
.emacs file.

    (setq yas/snippet-dirs '("c:/src/itasca-emacs/snippets"))

### Auto-complete mode

If you have `auto-complete-mode` installed FISH function names will be
auto-completed.

    (require 'auto-complete-config)
    (ac-config-default)

    (add-to-list 'ac-dictionary-directories "c:/src/itasca-emacs/ac-dict")

    (progn
      (add-to-list 'ac-modes 'itasca-general-mode)
      (add-to-list 'ac-modes 'itasca-pfc-mode)
      (add-to-list 'ac-modes 'itasca-flac-mode)
      (add-to-list 'ac-modes 'itasca-flac3d-mode)
      (add-to-list 'ac-modes 'itasca-udec-mode))
