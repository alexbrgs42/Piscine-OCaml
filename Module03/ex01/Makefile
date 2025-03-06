########################### BASE ###########################

NAME = value

SOURCES = Value.ml main.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt

opt = $(NAME).opt
byt = $(NAME).byt

########################### RULES ###########################

all:	$(NAME)

$(NAME): $(opt) $(byt)
	rm -f $(NAME)
	ln -s $(NAME).byt $(NAME)

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

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
	rm -f $(NAME).byt
	rm -f $(NAME).opt
	rm -f $(NAME)

re:	fclean all

.PHONY: all clean fclean re
