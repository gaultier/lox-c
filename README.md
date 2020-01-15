# lox-c


A compiler and bytecode vm for the programming language lox.


## Quickstart

Prerequisites:
- A C99 compiler
- POSIX Make
- GNU parallel (optional, only to run the tests)

```sh
$ make release
$ ./lox-release run hello.lox
$ printf "(12 + 2) * 3" | ./lox-release run -
$ ./lox-release dump hello.lox
```


## Development

**Recommended setup**


```sh
# With Adress Sanitizer:
$ make debug
$ Or without:
$ make debug SANITIZER=""
$ ./lox-debug
# Test
$ ./test.sh
```

## Docker

`make docker`

## Major differences with the official implementation

- Strings are not interned

## License

MIT
