.POSIX:
.SUFFIX:

CFLAGS= -std=c99 -Wall -Wextra -Wpedantic
SANITIZER= -fsanitize=address

SRC= hashtab.c lex.c main.c parse.c utils.c value.c vm.c

release: 
	$(CC) $(CFLAGS) $(LDFLAGS) $(SRC) -O3 -o lox-release

debug: 
	$(CC) $(CFLAGS) $(LDFLAGS) $(SRC) $(SANITIZER) -O0 -g -o lox-debug

docker:
	docker build -t lox-c .

clean:
	rm -rf lox-release lox-debug lox-debug.dSYM

.PHONY: clean docker
