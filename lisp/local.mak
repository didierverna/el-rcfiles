### local.mak --- Local lisp/ makefile for Emacs Lisp libraries

## Copyright (C) 2007, 2012, 2013 Didier Verna

## Author: Didier Verna <didier@didierverna.net>

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

www-dist:

xp-dist:
ifeq ($(SIMPLE),)
	echo "Please call xp-dist from the toplevel directory."
else
	install -m 644 $(EL_FILES) $(XP_DIR)/$(XP_PKG)/
endif

elpa-dist:
ifeq ($(SIMPLE),)
	echo "Please call elpa-dist from the toplevel directory."
else
	$(EMACS) --kill -nw --eval "(require 'package-x)" \
			    --eval '(package-upload-file "$(EL_FILES)")'
endif

update-version:
	perl -pi -e 's/;; Version: .*/;; Version: $(VERSION)/' $(EL_FILES)
	perl -pi -e 's/defvar $(PROJECT)-version .*/defvar $(PROJECT)-version "$(VERSION)"/' $(EL_FILES)


.PHONY: update-version www-dist elpa-dist


### local.mak ends here
