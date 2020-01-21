# lox-c


A compiler and bytecode vm for the programming language lox.


## Quickstart

Prerequisites:
- A C99 compiler
- POSIX Make

```sh
$ make lox-release
$ ./lox-release run test/hello_world.lox
$ printf "(12 + 2) * 3" | ./lox-release run -
$ ./lox-release dump test/hello_world.lox
```


## Development

**Recommended setup**

Additional prerequisites:

- GNU parallel (optional, only to run the tests)

```sh
# With Address Sanitizer:
$ make lox-debug
# Or without:
$ make lox-debug SANITIZER=""
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
