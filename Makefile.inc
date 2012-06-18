### Makefile.inc --- toplevel configuration makefile for XEmacs packages

# Path to your version of XEmacs:
EMACS := emacs

# Location of the package directory:
# GNU Emacs:
PKG_DIR := /usr/local/share/emacs/site-packages
# XEmacs 21.4:
# PKG_DIR := /usr/local/lib/xemacs/site-packages
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
