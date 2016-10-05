#
# Pure OCaml, no packages, no _tags
#

# bin-annot is required for Merlin and other IDE-like tools

OCB_FLAGS = -tag bin_annot -use-ocamlfind
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native byte # profile debug

clean:
			$(OCB) -clean

native:
			$(OCB) main.native

byte:
			$(OCB) main.byte

profile:
			$(OCB) -tag profile main.native

debug:
			$(OCB) -tag debug main.byte
sanity:
# check that packages can be found
			ocamlfind query yojson

test: 		native
			./main.native "OCaml" "OCamlBuild" "users"

.PHONY: 	all clean byte native profile debug test
