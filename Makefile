.POSIX:
CFLAGS = -std=c99 -O3 -g3 -Weverything

lox: main.c
	$(CC) $(CFLAGS) -o $@ $^ 

clean:
	rm -f a.out
