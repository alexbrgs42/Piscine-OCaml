############################ BASE ############################"

NAME = deck

SOURCES = Deck.ml main.ml
INTERFACE = Deck.mli

CAMLC = ocamlc
CAMLOPT = ocamlopt

opt = $(NAME).opt
byt = $(NAME).byt

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)
CMI = $(INTERFACE:.mli=.cmi)

############################ RULES ############################

all: $(NAME)

$(NAME):	$(opt) $(byt)
	rm -f $(NAME)
	ln -s $(NAME).byt $(NAME)

############################ LINK ############################

$(NAME).byt:	$(CMI) $(OBJS)
	$(CAMLC) -o $(NAME).byt $(OBJS)

$(NAME).opt:	$(CMI) $(OPTOBJS)
	$(CAMLOPT) -o $(NAME).opt $(OPTOBJS)

############################ COMPILATION ############################

.SUFFIXES:
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) -c $<

.mli.cmi:
	$(CAMLC) -c $<

.ml.cmx:
	$(CAMLOPT) -c $<

############################ CLEAN ############################

clean:
	rm -f $(OBJS)
	rm -f $(OPTOBJS)
	rm -f $(CMI)
	rm -f $(SOURCES:.ml=.o)
	rm -f $(SOURCES:.ml=.cmi)

fclean:	clean
	rm -f $(NAME)
	rm -f $(NAME).byt
	rm -f $(NAME).opt

re:	fclean all

.PHONY:	all clean fclean re
