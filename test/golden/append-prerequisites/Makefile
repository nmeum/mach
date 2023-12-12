CC ::= cc
CFLAGS ::= -O0
LDFLAGS ::= 
CPPFLAGS ::= 

all: main
	touch myheader.h
	$(MAKE) main

main: main.o myfunc.o
	$(CC) -o $@ $^

main.o myfunc.o: myheader.h
