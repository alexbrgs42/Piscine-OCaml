################## BASE ##################

NAME = ohW

SOURCES = people.ml main.ml

CAMLOPT = ocamlopt

################## EXEC ##################

all: $(NAME)


OBJ = $(SOURCES:.ml=.cmx)

################## LINK ##################

$(NAME):	$(OBJ)
	$(CAMLOPT) -o $(NAME) $(OBJ)

################## COMPILE ##################

.SUFFIXES:
.SUFFIXES: .ml .cmx

.ml.cmx:
	$(CAMLOPT) -c $<

################## CLEAN ##################

clean:
	rm -f $(OBJ)
	rm -f $(SOURCES:.ml=.cmi)
	rm -f $(SOURCES:.ml=.o)

fclean:	clean
	rm -f $(NAME)

re:	fclean all

.PHONY: all clean fclean re