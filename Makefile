.POSIX:
CFLAGS_DEBUG = -std=c99 -O0 -g3 -Weverything -fsanitize=address
CFLAGS_RELEASE = -std=c99 -O3 -Weverything

debug: main.c
	$(CC) $(CFLAGS_DEBUG) -o lox-debug $^

release: main.c
	$(CC) $(CFLAGS_RELEASE) -o lox-release $^

clean:
	rm -f lox-debug lox-release
