CC := gcc
CFLAGS := -O2 -Werror

SOURCES := main.c myfunc.c
OBJECTS := $(SOURCES:.c=.o)

.c.o:
	$(CC) $(CFLAGS) -c $^

main: $(OBJECTS)
	$(CC) -o main $(OBJECTS)
