export OCAMLMAKEFILE = ./OCamlMakefile

export LIBS=unix

define PROJ_client
	RESULT =reversi
	SOURCES=color.ml command.ml commandParser.mly commandLexer.mll bitboard.ml openingbook.ml play.ml main.ml
endef
export PROJ_client
define PROJ_server
	RESULT =reversi-serv
	SOURCES=color.ml command.ml commandParser.mly commandLexer.mll bitboard.ml openingbook.ml play.ml server.ml
endef
export PROJ_server

ifndef SUBPROJS
  export SUBPROJS = client server
endif

all: native-code

%:
	@$(MAKE) -f $(OCAMLMAKEFILE) subprojs SUBTARGET=$@
