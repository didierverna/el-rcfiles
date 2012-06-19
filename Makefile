### Makefile --- Generic toplevel makefile for Emacs libraries

## Copyright (C) 2008, 2009, 2010, 2011, 2012 Didier Verna
## Copyright (C) 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007 Didier Verna

## Author:     Didier Verna <didier@xemacs.org>
## Maintainer: Didier Verna <didier@xemacs.org>

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

## Please use GNU Make with this makefile.


### Code:

hack: all

include Makefile.prj
include Makefile.inc

-include local.prj
-include local.inc

all:
	$(MAKE) gen TARGET=all

gen:
	@for i in $(SUBDIRS) ; do                 \
	   echo "making $(TARGET) in $${i} ..." ; \
	   ( cd $${i} && $(MAKE) $(TARGET) ) ;    \
	 done

# [un]install-pkg clean distclean
.DEFAULT:
	$(MAKE) gen TARGET=$@

.PHONY: all gen				\
	Makefile.prj Makefile.inc	\
	local.prj local.inc local.mak

-include local.mak


### Makefile ends here
