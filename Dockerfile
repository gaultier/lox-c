FROM alpine as builder

RUN apk update && apk add meson gcc ninja build-base

WORKDIR /lox-c

COPY meson.build ./
COPY *.h ./
COPY *.c ./
# Ensure no logs
RUN echo "" > config.h

RUN meson build --buildtype=release
RUN ninja -C build

FROM alpine as runner
COPY --from=builder /lox-c/build/lox /usr/bin/lox

CMD ["lox", "repl"]
