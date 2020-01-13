FROM alpine as builder

RUN apk update && apk add meson gcc build-base

WORKDIR /lox-c

COPY Makefile ./
COPY *.h ./
COPY *.c ./

RUN make release

FROM alpine as runner
COPY --from=builder /lox-c/lox-release /usr/bin/lox

CMD ["lox", "repl"]
