
SOURCES:=\
	util.ml \
	configuration.ml \
	gtk_ext.ml \
	draw_tree.ml \
	proof_window.ml \
	main.ml

TOCLEAN+=prooftree
prooftree: $(SOURCES)
	ocamlopt.opt -I +lablgtk2 -o prooftree unix.cmxa lablgtk.cmxa \
		gtkInit.cmx $(SOURCES)

.PHONY: test
test: prooftree
	./prooftree -geometry +0+0

clean:
	rm -f $(TOCLEAN)
	rm -f *.cmi *.cmo *.cmx *.o *.cma *.cmxa *.a
