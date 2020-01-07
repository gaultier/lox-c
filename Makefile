.POSIX:
CFLAGS = -std=c99 -O0 -g3 -Weverything -fsanitize=address

lox: main.c
	$(CC) $(CFLAGS) -o $@ $^ 

clean:
	rm -f a.out
