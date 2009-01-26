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
	clean		\
	clean-slides	\
	slides

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

clean: clean-slides
	rm -rf _build

clean-slides:
	rm -rf	doc/*.aux	\
		doc/*.bbl	\
		doc/*.blg	\
		doc/*.log	\
		doc/*.mpx	\
		doc/*.nav	\
		doc/*.out	\
		doc/*.pdf	\
		doc/*.snm	\
		doc/*.toc	\
		doc/mpxerr.tex


distclean: clean distclean-slides
	rm -rf doc/dot doc/html doc/latex doc/man doc/texi *.native *.byte

distclean-slides:
	rm -rf doc/*.pdf doc/*.1

doc: doc-dot doc-html doc-latex doc-man doc-texi

slides:
	cd doc && mpost fig1.mp && mpost fig2.mp && texi2pdf mclml-slides.tex

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
