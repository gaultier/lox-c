# lox-c


A compiler and bytecode vm for the programming language lox.


## Quickstart

Prerequisites:
- A C99 compiler
- Meson (`pip3 install --user meson`)
- Ninja

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
$ meson build -Dwith_logs=true -Db_sanitize=address 
$ ninja -C build
$ ./build/lox
```

## Docker

`docker build -t lox-c .`

## Major differences with the official implementation

- Strings are not interned

## License

MIT
