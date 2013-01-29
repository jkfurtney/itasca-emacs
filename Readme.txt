Emacs modes for editing Itasca software data files.
---------------------------------------------------
FLAC FLAC3D UDEC 3DEC PFC www.itascacg.com/software

Installation: copy this file somwhere on the emacs load-path and
add (load "itasca.el") to your .emacs file.

To set the mode on a per-file basis: put a comment in the following
form at the top of the file.

;; -*- mode: itasca-general -*-
;; -*- mode: itasca-flac -*-
;; -*- mode: itasca-flac3d -*-
;; -*- mode: itasca-pfc -*-
