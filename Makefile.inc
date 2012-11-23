### Makefile.inc --- Generic toplevel include makefile for Emacs Lisp libraries

## Copyright (C) 2011, 2012 Didier Verna

## Author:     Didier Verna <didier@didierverna.net>
## Maintainer: Didier Verna <didier@didierverna.net>

# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.


### Commentary:


### Code:

# Path to your version of Emacs:
EMACS := emacs

# Prefix for standard GNU Emacs installation with `make install'.
# Installation will use the following subdirectories:
# PREFIX/share/emacs/site-lisp/
# PREFIX/info/
# PREFIX/etc/
PREFIX := /usr/local

# Package prefix and directory for (X)Emacs package installation.
# Installation will use the following subdirectories:
# PKG_DIR/lisp/<project>/
# PKG_DIR/info/
# PKG_DIR/etc/<project>/
# Note that GNU Emacs doesn't really understand this "package"
# notion. You need some stuff in your initialization file to make it work.
PKG_PREFIX := $(PREFIX)
# GNU Emacs:
PKG_DIR := $(PKG_PREFIX)/share/emacs/site-packages
# XEmacs 21.4:
# PKG_DIR := $(PKG_PREFIX)/lib/xemacs/site-packages
# XEmacs 21.5:
# PKG_DIR := $(PKG_PREFIX)/share/xemacs/site-packages

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
