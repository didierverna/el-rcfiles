### Makefile --- Generic lisp makefile for XEmacs packages

## Copyright (C) 2008, 2009, 2010, 2011 Didier Verna
## Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007 Didier Verna

## Author:        Didier Verna <didier@xemacs.org>
## Maintainer:    Didier Verna <didier@xemacs.org>
## Created:       Wed Mar  1 13:25:19 2000
## Last Revision: Mon Jun 18 17:41:04 2012

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License version 2,
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

## Please use GNU make with this makefile.


### Code:

hack: all

include  ../Makefile.prj
include  ../Makefile.inc

-include ../local.prj
-include ../local.inc

include  Makefile.prj
-include local.inc

XEMACS      := $(shell if $(EMACS) --version \
			    | grep "XEmacs" > /dev/null; then		\
			 echo t;					\
		       fi)
XEMACS_21_5 := $(shell if $(EMACS) --version \
			    | grep "XEmacs 21.5" > /dev/null; then	\
			 echo t;					\
		       fi)

ifeq ($(XEMACS),t)
AUTOLOAD_FILE := auto-autoloads.el
CUSLOAD_FILE  := custom-load.el
else
AUTOLOAD_FILE := $(PROJECT)-autoloads.el
CUSLOAD_FILE  := cus-load.el
endif

GEN_EL_FILES := $(AUTOLOAD_FILE) $(CUSLOAD_FILE)
ifeq ($(XEMACS_21_5),t)
GEN_EL_FILES += custom-defines.el
endif


# #### NOTE: the lisp files are not detected automatically. They must be
# listed explicitely in Makefile.prj (which can, in turn detect them
# automatically), because the compilation order might be important (e.g. for
# getting macros).
ALL_EL_FILES := $(EL_FILES) $(GEN_EL_FILES)
ELC_FILES    := $(ALL_EL_FILES:%.el=%.elc)
EL_DIR       := $(PKG_DIR)/lisp/$(PROJECT)

# For the compiler to find the require'd files:
ifeq ($(XEMACS),t)
ADD_MY_PATH := -eval '(push "." load-path)'
else
ADD_MY_PATH := --directory .
endif

#### FIXME: this is here because of Clox that needs to modify ELC_FILES, but
#### it shouldn't be.
-include Makefile.inc

ifeq ($(XEMACS_21_5),t)
all: autoloads custom-load custom-defines elc
else
all: autoloads custom-load elc
endif

autoloads: $(AUTOLOAD_FILE)

custom-load: $(CUSLOAD_FILE)

ifeq ($(XEMACS_21_5),t)
custom-defines: custom-defines.el
endif

elc: $(ELC_FILES)

install-pkg: $(ELC_FILES)
	install -d -m 755 $(EL_DIR)
ifeq ($(SYMLINK),)
	install -m 644 $(ALL_EL_FILES) $(ELC_FILES) $(EL_DIR)/
else
	ln -fs $(abspath $(ALL_EL_FILES)) $(abspath $(ELC_FILES)) $(EL_DIR)/
endif

uninstall-pkg:
	test -d $(EL_DIR) && rm -fr $(EL_DIR)

clean:
	-rm *~

distclean: clean
	-rm -f *.elc $(AUTOLOAD_FILE) $(CUSLOAD_FILE)
ifeq ($(XEMACS_21_5),t)
	-rm custom-defines.el
endif

$(AUTOLOAD_FILE): $(EL_FILES)
ifeq ($(XEMACS_21_5),t)
	$(EMACS) -batch -l autoload -f batch-update-directory-autoloads \
	  $(PROJECT) .
else ifeq ($(XEMACS),t)
	$(EMACS) -batch -q -l autoload				\
	  -eval '(setq autoload-package-name "$(PROJECT)")'	\
	  -f batch-update-directory .
else
	$(EMACS) --batch --load autoload	\
	  --eval '(let ((generated-autoload-file \
			 "$(abspath $(AUTOLOAD_FILE))"))\
		    (update-directory-autoloads "."))'
endif
	@rm -f $@~
	@touch $@

$(CUSLOAD_FILE): $(EL_FILES)
ifeq ($(XEMACS),t)
	$(EMACS) -batch -q -l cus-dep -f Custom-make-dependencies .
else
	$(EMACS) --batch --load cus-dep -f custom-make-dependencies .
endif
	@rm -f $@~
	@touch $@

ifeq ($(XEMACS_21_5),t)
custom-defines.el: $(EL_FILES)
	$(EMACS) -batch -l autoload -f batch-update-directory-custom-defines \
	  $(PROJECT) .
	@rm -f $@~
	@touch $@
endif


%.elc: %.el
ifeq ($(XEMACS),t)
	$(EMACS) -batch -q $(ADD_MY_PATH) -f batch-byte-compile $<
else
	$(EMACS) --batch $(ADD_MY_PATH) -f batch-byte-compile $<
endif

.PHONY: all autoloads custom-load custom-defines			\
	elc								\
	install-pkg uninstall-pkg clean distclean			\
	../Makefile.prj ../Makefile.inc Makefile.prj Makefile.inc	\
	../local.prj ../local.inc local.inc local.mak

-include local.mak

### Makefile ends here
