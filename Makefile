RM			=	rm -f
PRINT		=	printf

CAMLFLAGS	=	-w Aelz -warn-error A
OCAMLC		=	ocamlc $(CAMLFLAGS)
OCAMLOPT	=	ocamlopt $(CAMLFLAGS)
OCAMLLEX	=	ocamllex
MENHIR		=	menhir

NAME		=	bistro

ML			=	bigint.ml			\
				biginttest.ml		\
				arithexpr.ml		\
				arithexprtest.ml	\
				lexer.ml			\
				parser.ml			\
				bistroreloaded.ml	\
				main.ml

MLI			=	bigint.mli			\
				biginttest.mli		\
				arithexpr.mli		\
				arithexprtest.mli	\
				bistroreloaded.mli	\
				parser.mli

MLL			=	lexer.mll

MLY			=	parser.mly

CMX			=	$(ML:.ml=.cmx)

CMO			=	$(ML:.ml=.cmo)

CMI			=	$(MLI:.mli=.cmi)

CML			=	$(MLL:.mll=.ml) $(MLY:.mly=.ml) $(MLY:.mly=.mli)

%.cmx		:	%.ml
				$(OCAMLOPT) -c $<

%.cmo		:	%.ml
				$(OCAMLC) -c $<

%.cmi		:	%.mli
				$(OCAMLC) -c $<

%.ml		:	%.mll
				$(OCAMLLEX) $<
				@$(PRINT) "\033[36m*** Lexer is generated by $(OCAMLLEX) ***\n\033[00m"

%.ml		:	%.mly
				$(MENHIR) $<
				@$(PRINT) "\033[36m*** Parser is generated by $(MENHIR) ***\n\033[00m"

%.mli		:	%.mly
				$(MENHIR) $<

all			:	$(CML) $(CMI) $(NAME)

byte		:	$(CML) $(CMI) $(NAME).byte

$(NAME)		:	$(CMX)
				$(OCAMLOPT) -o $@ unix.cmxa $(CMX)
				@$(PRINT) "\033[32m*** Binary $@ is linked ***\n\033[00m"

$(NAME).byte:	$(CMO)
				$(OCAMLC) -o $@ unix.cma $(CMO)
				@$(PRINT) "\033[32m*** Binary $@ is linked ***\n\033[00m"

clean		:
				$(RM) *.cm* *.o
				$(RM) *~ \#*\#
				@$(PRINT) "\033[31m*** Objects are removed ***\n\033[00m"

fclean		:	clean
				$(RM) lexer.ml parser.ml parser.mli
				$(RM) $(NAME) $(NAME).byte
				@$(PRINT) "\033[31m*** Binarys, Lexer and Parser are removed ***\n\033[00m"

re			:	fclean all

.PHONY		:	all byte clean fclean re