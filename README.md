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
$ meson build -Db_sanitize=address 
$ ninja -C build
# Run
$ ./lox
# Test
$ ./test.sh
```

## Docker

`docker build -t lox-c .`

## Major differences with the official implementation

- Strings are not interned

## License

MIT
