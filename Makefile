PREFIX   ?= /usr/local
OBJDIR    = obj
LIBDIR   ?= lib
SRCDIR    = src
DESTDIR   ?=
GPR_FILES = gnat/*.gpr
LIBRARY_TYPE = dynamic

MAJOR   = 0
MINOR   = 2
REV     = 2
VERSION = $(MAJOR).$(MINOR).$(REV)
DBUSADA = libadabasexclient-$(VERSION)

SO_LIBRARY   = libadabasexclient.so.$(VERSION)

PREPARE := $(shell mkdir -p $(OBJDIR) $(LIBDIR))

all: build_lib gpr

build_lib:
	gnatmake -Padabasexclient

gpr:
	gnatprep -b gnat/adabasexclient.gpr.gp gnat/adabasexclient.gpr -DInvoked_By_Makefile \
       '-DIncludedir="$(PREFIX)/include"' '-DLibdir="$(PREFIX)/$(LIBDIR)"' \
       '-DAlidir="$(PREFIX)/$(LIBDIR)"' '-DLibrary_Type="${LIBRARY_TYPE}"'

build_examples:
	gnatmake -Padabasexclient_examples

build_check:    
	gnatmake -Padabasexclient_tests

install: install_lib

install_lib: build_lib
	install -d $(DESTDIR)$(PREFIX)/include/adabasexclient
	install -d $(DESTDIR)$(PREFIX)/$(LIBDIR)/adabasexclient
	install -d $(DESTDIR)$(PREFIX)/share/gpr
	install -m 644 $(SRCDIR)/*.ad[bs] $(DESTDIR)$(PREFIX)/include/adabasexclient
	install -m 444 $(LIBDIR)/*.ali $(DESTDIR)$(PREFIX)/$(LIBDIR)/adabasexclient
	install -m 644 $(GPR_FILES) $(DESTDIR)$(PREFIX)/share/gpr
	install -m 644 $(LIBDIR)/$(SO_LIBRARY) $(DESTDIR)$(PREFIX)/$(LIBDIR)
	cd $(DESTDIR)$(PREFIX)/$(LIBDIR) && ln -sf $(SO_LIBRARY) libadabasexclient.so

test: build_lib build_check
	./runner

clean:
	@rm -rf $(OBJDIR)
	@rm -rf $(LIBDIR)
	@rm -rf example exampleadd examplecreate
	@rm -rf queryexample querybindexample
	@rm -rf runner

