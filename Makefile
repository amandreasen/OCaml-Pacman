#add ghost and state once defined
MODULES=player map test state
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

zip:
	zip finalproject.zip *.ml* INSTALL.TXT _tags Makefile

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private adventure.zip
