##
# Macros
##

AR = ar
ARFLAGS = -rv
YACC = yacc
#YFLAGS =
LEX = lex
#LFLAGS =
#LDFLAGS =
CC = c17
CFLAGS = -O 1

##
# Single Suffix Rules
##

# To-Do

##
# Double Suffix Rules
##

## TODO: Use $< instead of $^ here

.c.o:
	$(CC) $(CFLAGS) -c $^

.y.o:
	$(YACC) $(YFLAGS) $^
	$(CC) $(CFLAGS) -c y.tab.c
	rm -f y.tab.c
	mv y.tab.o $@

.l.o:
	$(LEX) $(LFLAGS) $^
	$(CC) $(CFLAGS) -c lex.yy.c
	rm -f lex.yy.c
	mv lex.yy.o $@

.y.c:
	$(YACC) $(YFLAGS) $^
	mv y.tab.c $@

.l.c:
	$(LEX) $(LFLAGS) $^
	mv lex.yy.c $@
