### local.inc --- Local toplevel include makefile for Emacs Lisp libraries

## Copyright (C) 2012, 2013 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

# Copying and distribution of this file, with or without modification,
# are permitted in any medium without royalty provided the copyright
# notice and this notice are preserved.


### Commentary:


### Code:


DIST_NAME := el-$(PROJECT)-$(VERSION)
TARBALL   := $(DIST_NAME).tgz
SIGNATURE := $(TARBALL).asc

WWW_HOST := www
WWW_DIR := ~/www/software/elisp/$(PROJECT)

XP_DIR := /usr/local/share/emacs-lisp/source/xemacs-packages/xemacs-packages
XP_PKG := xemacs-base


## Local Variables:
## mode: makefile
## End:

### local.inc ends here
