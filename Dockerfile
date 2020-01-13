FROM alpine as builder

RUN apk update && apk add meson gcc ninja build-base

WORKDIR /lox-c

COPY meson.build ./
COPY meson_options.txt ./
COPY *.h ./
COPY *.c ./

RUN meson build --buildtype=release
RUN ninja -C install

FROM alpine as runner
COPY --from=builder `which lox` /usr/bin/lox

CMD ["lox", "repl"]
