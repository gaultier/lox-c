.POSIX:
.SUFFIX:

CFLAGS= -std=c99 -Wall -Wextra -Wpedantic -Wsign-conversion -Wmissing-prototypes
SANITIZER= -fsanitize=address

SRC_H= buf.h config.h hashtab.h lex.h parse.h result.h utils.h value.h vm.h
SRC_C= hashtab.c lex.c main.c parse.c utils.c value.c vm.c

release: $(SRC_C) $(SRC_H)
	$(CC) $(CFLAGS) $(LDFLAGS) $(SRC_C) -O3 -o lox-release

debug: $(SRC_C) $(SRC_H)
	$(CC) $(CFLAGS) $(LDFLAGS) $(SRC_C) $(SANITIZER) -O0 -g -o lox-debug

docker:
	docker build -t lox-c .

clean:
	rm -rf lox-release lox-debug lox-debug.dSYM

.PHONY: clean docker
