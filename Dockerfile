FROM alpine as builder

RUN apk update && apk add gcc build-base git readline-dev curl bash

RUN adduser -D xmake

USER xmake

RUN curl -fsSL https://raw.githubusercontent.com/tboox/xmake/master/scripts/get.sh | bash

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
