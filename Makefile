SOURCES = chars.ml config.ml asciipict.ml
RESULT = asciipict
LIBS = graphics str
PREFIX = $(HOME)

all: native-code

archive:
	rm -rf /tmp/asciipict
	cp -r . /tmp/asciipict
	cd /tmp/asciipict && make clean
	cd /tmp && tar czf asciipict.tar.gz asciipict && \
           tar cjf asciipict.tar.bz2 asciipict
	mv /tmp/asciipict.tar.gz /tmp/asciipict.tar.bz2 .

install: all
	cp -pf $(RESULT) $(PREFIX)/bin

TRASH = *~ asciipict.tar.gz asciipict.tar.bz2

OCAMLMAKEFILE = OcamlMakefile
include $(OCAMLMAKEFILE)
