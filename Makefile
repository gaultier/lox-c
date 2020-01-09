.POSIX:
CFLAGS_DEBUG = -std=c99 -O0 -g3 -Wall -Wextra -Wpedantic -fsanitize=address
CFLAGS_RELEASE = -std=c99 -O3 -Wall -Wextra -Wpedantic

debug: main.c
	$(CC) $(CFLAGS_DEBUG) -o lox-debug $^

release: main.c
	$(CC) $(CFLAGS_RELEASE) -DNDEBUG -o lox-release $^

clean:
	rm -f lox-debug lox-release
