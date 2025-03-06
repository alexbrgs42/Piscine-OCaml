########################### BASE ###########################

NAME = card

SOURCES = Card.ml main.ml

CAMLC = ocamlc
CAMLOPT= ocamlopt

opt = $(NAME).opt
byt = $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

########################### RULES ###########################

all : $(NAME)

$(NAME):	$(opt) $(byt)
	rm -f $(NAME)
	ln -s $(NAME).byt $(NAME)

########################### LINK ###########################

$(NAME).byt:	$(OBJS)
	$(CAMLC) -o $(NAME).byt $(OBJS)

$(NAME).opt:	$(OPTOBJS)
	$(CAMLOPT) -o $(NAME).opt $(OPTOBJS)

########################### COMPILATION ###########################

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

########################### CLEAN ###########################

clean:
	rm -f $(OBJS)
	rm -f $(OPTOBJS)
	rm -f $(SOURCES:.ml=.cmi)
	rm -f $(SOURCES:.ml=.o)

fclean:	clean
	rm -f $(NAME)
	rm -f $(NAME).byt
	rm -f $(NAME).opt

re:	fclean all

.PHONY: all clean fclean re