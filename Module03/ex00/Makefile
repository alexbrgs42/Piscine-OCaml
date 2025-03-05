########################### BASE ###########################

NAME = color

SOURCES = Color.ml main.ml

CAMLC = ocamlc
CAMLOPT = ocamlopt

opt = $(NAME).opt
byt = $(NAME).byt

# cclib permet de linker la lib .a avec le .cma correspondant
# LIBS = $(WITHGRAPHICS)
# WITHGRAPHICS = graphics.cma -cclib -lGraphics

########################### RULES ###########################

all : $(NAME)

$(NAME): $(opt) $(byt)
	rm -f $(NAME)
	ln -s $(NAME).byt $(NAME)

OBJS = $(SOURCES:.ml=.cmo)
OPTOBJS = $(SOURCES:.ml=.cmx)

########################### LINK ###########################

$(NAME).byt:	$(OBJS)
	$(CAMLC) -o $(NAME).byt $(LIBS) $(OBJS)

$(NAME).opt:	$(OPTOBJS)
	$(CAMLOPT) -o $(NAME).opt $(LIBS) $(OPTOBJS)

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
	rm -f $(NAME).o

fclean:	clean
	rm -f $(NAME)
	rm -f $(NAME).byt
	rm -f $(NAME).opt

re:	fclean all

.PHONY = all clean fclean re
