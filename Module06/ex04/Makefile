################### BASE ###################

NAME = leW

CAMLOPT = ocamlopt

SOURCES = people.ml doctor.ml dalek.ml army.ml galifrey.ml main.ml

OBJ = $(SOURCES:.ml=.cmx)

################### RULES ###################

all:	$(NAME)

################### COMPILATION ###################

$(NAME):	$(OBJ)
	$(CAMLOPT) -o $(NAME) $(OBJ)

################### LINK ###################

.SUFFIXES:
.SUFFIXES:	.ml .cmx

.ml.cmx:
	$(CAMLOPT) -c $<

################### CLEAN ###################

clean:
	rm -f $(OBJ)
	rm -f $(SOURCES:.ml=.cmi)
	rm -f $(SOURCES:.ml=.o)

fclean:
	rm -f $(NAME)

re:	fclean all

.PHONY:	all clean fclean re
