# OCAML

## 📖 About

This project is called a Piscine. Within 9 modules, each composed of multiple exercices, I explored a large panel of tools available in OCaml that I listed bellow. 

**<ins>Why OCaml ?</ins>**

I discovered OCaml earlier in my studies and really enjoyed pattern matching that I found so powerfull and elegant. 

**<ins>What did I gain ?</ins>**

I also was able to have a better understanding of functional programming and I discussed a lot about programming paradigm with my peers. I gained a better culture and I have a lot more tools, algorithms and perspectives which I find essential in dev.

## 📚 Modules

• [Module00](https://github.com/alexbrgs42/Piscine-OCaml/tree/main/Module00) - Basic syntax and semantics\
• [Module01](https://github.com/alexbrgs42/Piscine-OCaml/tree/main/Module01) - Recursion and higher-order functions\
• [Module02](https://github.com/alexbrgs42/Piscine-OCaml/tree/main/Module02) - Pattern Matching and Data Types\
• [Module03](https://github.com/alexbrgs42/Piscine-OCaml/tree/main/Module03) - OCaml’s modules language\
• [Module04](https://github.com/alexbrgs42/Piscine-OCaml/tree/main/Module04) - Imperative features\
• [Module05](https://github.com/alexbrgs42/Piscine-OCaml/tree/main/Module05) - Functor\
• [Module06](https://github.com/alexbrgs42/Piscine-OCaml/tree/main/Module06) - Object oriented programming\
• [Module07](https://github.com/alexbrgs42/Piscine-OCaml/tree/main/Module07) - Object oriented programming\
• [Module08](https://github.com/alexbrgs42/Piscine-OCaml/tree/main/Module08) - Monoids and Monads

## 📌 Memo

 .ml  : source code\
 .mli : interface file\
 .cmi : compiled interface file\
 .cmo : object bytecode\
 .cmx : native compilation info\
 .o   : native code compiled\
 
 .ml + .mli = a compilation unit\
 .cma = lib\
 .cmxa = compiled lib

build order :\
 1 - source with ocamlc/ocamlopt -c test.ml\
 2 - then we link with ocamlc/ocamlopt

## 📚 Ressources

- Object Oriented Programming : [Video](https://elearning.intra.42.fr/notions/piscine-ocaml-d04-modules-language/subnotions/piscine-ocaml-d04-modules-language-7-modular-programming-vs-object-oriented-programming/videos/313)
- Brian Beckman: Don't fear the Monad : [Amazing video !](https://www.youtube.com/watch?v=ZhuHCtR3xq8)

## 🛠️ Installation

```bash
# Clone the repository
git clone git@github.com:alexbrgs42/Piscine-OCaml.git
cd Piscine-OCaml
```

## 📈 Build and 🏃 run

You can then change directory into the Module you want to look for !

Some exercices have a Makefile, others don't (depending on the assignment). If needed you can simply compile every source code of the exercise with the compiler `ocamlopt` like this :

```bash
cd Module00
cd ex00
# ! Here you need to decomment the main !
ocamlopt ft_test_sign.ml
```

Finally you have an executable :

```bash
$ ./a.out
positive
positive
negative
```

Otherwise you only need to `make`:

```bash
$ make
$ ./a.out
positive
positive
negative
```
