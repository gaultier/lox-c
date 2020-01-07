.POSIX:
CFLAGS = -std=c99 -Wall -Wextra -O3 -g3 -Wpedantic -Wcovered-switch-default

lox: main.c
	$(CC) $(CFLAGS) -o $@ $^ 

clean:
	rm -f a.out
