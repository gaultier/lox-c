# lox-c


A bytecode vm for the programming language lox


## Quickstart

Prerequisites:
- A C99 compiler
- POSIX Make

```sh
$ make release
$ ./lox-release run hello.lox
$ printf "(12 + 2) * 3" | ./lox-release run -
$ ./lox-release dump hello.lox
```


## License

MIT
