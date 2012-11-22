### local.mak --- Generic toplevel local makefile for Emacs Lisp libraries

## Copyright (C) 2007, 2012 Didier Verna

## Author:     Didier Verna <didier@didierverna.net>
## Maintainer: Didier Verna <didier@didierverna.net>

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License version 3,
## as published by the Free Software Foundation.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program; if not, write to the Free Software
## Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


### Commentary:

## Contents management by FCM version 0.1.

## Please use GNU Make with this makefile.


### Code:

DIST_NAME := $(PROJECT)-$(VERSION)
TARBALL   := $(DIST_NAME).tgz
SIGNATURE := $(TARBALL).asc

tag:
	git tag 'version_$(VERSION)'

convert:
	hg convert --filemap .hgfilemap --authormap .hgauthormap \
	  . /usr/local/share/emacs-lisp/source/xemacs-packages/xemacs-packages/$(PROJECT)/

tgz: $(TARBALL)
gpg: $(SIGNATURE)
dist: tgz gpg

distclean:
	-rm $(TARBALL) $(SIGNATURE)
	$(MAKE) gen TARGET=distclean

install-www: dist
	echo "$(VERSION)" > $(WWW_DIR)/version.txt
	chmod 644 $(WWW_DIR)/version.txt
	install -m 644 NEWS $(WWW_DIR)/news.txt
	install -m 644 $(TARBALL)   "$(WWW_DIR)/attic/"
	install -m 644 $(SIGNATURE) "$(WWW_DIR)/attic/"
	echo "\
<? lref (\"$(PROJECT)/attic/$(TARBALL)\", \
	 contents (\"Dernière version\", \"Latest version\")); ?> \
| \
<? lref (\"$(PROJECT)/attic/$(SIGNATURE)\", \
	 contents (\"Signature GPG\", \"GPG Signature\")); ?>" \
	  > "$(WWW_DIR)/latest.txt"
	chmod 644 "$(WWW_DIR)/latest.txt"
	git push "$(WWW_DIR)/$(PROJECT).git"
	$(MAKE) gen TARGET=install-www
	cd "$(WWW_DIR)"					\
	  && ln -fs attic/$(TARBALL) latest.tar.gz	\
	  && ln -fs attic/$(SIGNATURE) latest.tar.gz.asc

uninstall-www:
	-rm -fr $(WWW_DIR)

$(TARBALL):
	git archive --format=tgz --prefix=$(DIST_NAME)/	-o $@ HEAD

$(SIGNATURE): $(TARBALL)
	gpg -b -a $<

.PHONY: tag convert tgz gpg dist distclean install-www uninstall-www


### local.mak ends here
