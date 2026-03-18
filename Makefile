DESTDIR =
PACKAGE_TARNAME = omp
prefix = /usr/local
exec_prefix = ${prefix}
datarootdir = ${prefix}/share
DOCDIR = ${datarootdir}/doc/omp
BINDIR = $(DESTDIR)${exec_prefix}/bin
SHAREDIR = $(datarootdir)/omp

# 1. We now use ocamlfind to call the compiler
OCAMLOPT = opam exec -- ocamlfind ocamlopt

# Use x86_64 as the directory name since the code expects it
MACHINE = x86_64
INSTALL = /usr/bin/install -c

# 2. ocamlfind automatically handles the .cmxa files, so we just tell it to link them
LIBS = -linkpkg 

SOURCES = qlib vlc g playlist confedit
RESULT = omp
CMX = $(patsubst %, %.cmx, $(SOURCES))

# 3. We use -package instead of -I to let ocamlfind locate LablTk, Unix, and Graphics automatically
OPTS = -g -thread -package labltk,unix,graphics -I $(MACHINE)

all: omp

omp: $(CMX) omp.ml
	mkdir -p $(MACHINE)
	cp omp.ml $(MACHINE)
	cd $(MACHINE); $(OCAMLOPT) $(OPTS) -o omp $(LIBS) $(CMX) omp.ml
	strip $(MACHINE)/omp

qlib.cmx: qlib.ml
	mkdir -p $(MACHINE)
	$(OCAMLOPT) $(OPTS) -c -o $(MACHINE)/$@ $^

vlc.cmx: qlib.cmx vlc.ml
	$(OCAMLOPT) $(OPTS) -c -o $(MACHINE)/$@ $^

g.cmx: qlib.cmx vlc.cmx g.ml
	$(OCAMLOPT) $(OPTS) -c -o $(MACHINE)/$@ $^

playlist.cmx: qlib.cmx vlc.cmx g.cmx playlist.ml
	$(OCAMLOPT) $(OPTS) -c -o $(MACHINE)/$@ $^

confedit.cmx: qlib.cmx vlc.cmx g.cmx playlist.cmx confedit.ml
	$(OCAMLOPT) $(OPTS) -c -o $(MACHINE)/$@ $^

install:
	sh mkompinit.sh $(SHAREDIR)
	$(INSTALL) -m 755 -d $(BINDIR) $(SHAREDIR) $(DOCDIR)
	$(INSTALL) -m 755 $(MACHINE)/omp ompinit $(BINDIR)
	$(INSTALL) -m 644 images/*.gif omprc $(SHAREDIR)
	$(INSTALL) -m 644 doc/ompdoc.pdf $(DOCDIR)

clean:
	rm -rf $(MACHINE)/*.{cmi,cmx,o} $(MACHINE)/omp
