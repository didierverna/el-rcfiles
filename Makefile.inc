### Makefile.inc --- toplevel configuration makefile for Emacs libraries

## Author:     Didier Verna <didier@xemacs.org>
## Maintainer: Didier Verna <didier@xemacs.org>

## Copyright (C) 2011, 2012 Didier Verna

# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.


### Commentary:


### Code:

# Path to your version of Emacs:
EMACS := emacs

# Prefix for standard GNU Emacs installation with `make install'. Installation
# will use the following subdirectories:
# PREFIX/share/emacs/site-lisp/
# PREFIX/info/
# PREFIX/etc/
PREFIX := /usr/local

# Package directory for installation as an (X)Emacs package. Note that support
# for GNU Emacs doesn't really work here (see INSTALL).
# GNU Emacs:
# PKG_DIR := /usr/local/share/emacs/site-packages
# XEmacs 21.4:
PKG_DIR := /usr/local/lib/xemacs/site-packages
# XEmacs 21.5:
# PKG_DIR := /usr/local/share/xemacs/site-packages

# Set to t if you want a symlink installation
SYMLINK :=

# Programs for generating the documentation:
MAKEINFO := makeinfo
TEXI2DVI := texi2dvi
DVIPS    := dvips


## Local Variables:
## mode: makefile
## End:

### Makefile.inc ends here
