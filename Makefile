.POSIX:
CFLAGS_DEBUG = -std=c99 -O0 -g3 -Wall -Wextra -pedantic -fsanitize=address
CFLAGS_RELEASE = -std=c99 -O3 -Wall -Wextra -pedantic

debug: main.c
	$(CC) $(CFLAGS_DEBUG) -o lox-debug main.c

release: main.c
	$(CC) $(CFLAGS_RELEASE) -DNDEBUG -o lox-release main.c

clean:
	rm -f lox-debug lox-release
