# lox-c


A compiler and bytecode vm for the programming language lox.


## Quickstart

Prerequisites:
- A C99 compiler
- Meson (`pip3 install --user meson`)
- Ninja
- GNU parallel (optional, only to run the tests)

```sh
$ meson build --buildtype=release
$ ninja -C build
$ ./build/lox run hello.lox
$ printf "(12 + 2) * 3" | ./lox-release run -
$ ./build/lox dump hello.lox
```


## Development

**Recommended setup**


```sh
# The last option will only work with clang
$ xmake config -m debug
$ xmake build
# Run
$ xmake run main repl
# Or
$ ./lox-release repl
# Test
$ ./test.sh
```

## Docker

`docker build -t lox-c .`

## Major differences with the official implementation

- Strings are not interned

## License

MIT
