PREFIX ?=/usr
OCAML_VERSION ?= 4.00.1

START = $(DESTDIR)$(PREFIX)
OCAML_LIBDIR ?= $(START)/lib/ocaml/
OCAML_FIND ?= ocamlfind

all: build

clean:
	ocamlbuild -clean

build:
	ocamlbuild -j 4 -use-ocamlfind edamandeve.native edamandeve.byte
