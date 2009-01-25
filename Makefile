.PHONY: all		\
	demo-native 	\
	demo-byte	\
	demo-debug	\
	doc 		\
	doc-dot		\
	doc-html	\
	doc-latex	\
	doc-man		\
	doc-texi	\
	clean

MKDIR = mkdir -p

OCAMLBUILD = ocamlbuild
OCAMLBUILD_FLAGS = -I src -lib graphics

OCAMLDOC = ocamldoc
OCAMLDOC_FLAGS = -I _build/src -I src


all: demo-native

demo-native:
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) demo/demo.native

demo-byte:
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) demo/demo.byte

demo-debug:
	$(OCAMLBUILD) $(OCAMLBUILD_FLAGS) -ocamlc ocamlcp demo/demo.byte

clean:
	rm -rf _build

distclean: clean
	rm -rf doc/dot doc/html doc/latex doc/man doc/texi *.native *.byte



doc: doc-dot doc-html doc-latex doc-man doc-texi

doc-dot: demo-native
	$(MKDIR) doc/dot && \
	$(OCAMLDOC) $(OCAMLDOC_FLAGS) -d doc/dot -dot src/*.ml \
	-o doc/dot/mclml.dot

doc-html: demo-native
	$(MKDIR) doc/html && \
	$(OCAMLDOC) $(OCAMLDOC_FLAGS) -d doc/html -html src/*.ml

doc-latex: demo-native
	$(MKDIR) doc/latex && \
	$(OCAMLDOC) $(OCAMLDOC_FLAGS) -d doc/latex -latex src/*.ml \
	-o doc/latex/mclml.tex

doc-man: demo-native
	$(MKDIR) doc/man && \
	$(OCAMLDOC) $(OCAMLDOC_FLAGS) -d doc/man -man src/*.ml

doc-texi: demo-native
	$(MKDIR) doc/texi && \
	$(OCAMLDOC) $(OCAMLDOC_FLAGS) -d doc/texi -texi src/*.ml
