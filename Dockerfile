FROM alpine as builder

WORKDIR /lox-c

COPY Makefile .
COPY *.h .
COPY *.c .

RUN make release

FROM alpine as runner
COPY --from=builder /lox-c/lox-release /usr/bin/lox

CMD ["lox", "repl"]
