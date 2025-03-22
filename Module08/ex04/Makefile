#################### base ####################

NAME = set

CAMLOPT = ocamlopt

SOURCES = main.ml

OBJ = $(SOURCES:.ml=.cmx)

#################### rules ####################

all:	$(NAME)

#################### compilation ####################

$(NAME):	$(OBJ)
	$(CAMLOPT) -o $(NAME) $(OBJ)

#################### link ####################

.SUFFIXES:
.SUFFIXES: .ml .cmx

.ml.cmx:
	$(CAMLOPT) -c $<

#################### clean ####################

clean:
	rm -f $(OBJ)
	rm -f $(SOURCES:.ml=.cmi)
	rm -f $(SOURCES:.ml=.o)

fclean:	clean
	rm -f $(NAME)

re:	fclean all

.PHONY: all clean fclean re
