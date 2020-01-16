FROM alpine as builder

RUN apk update && apk add make gcc build-base

WORKDIR /lox-c

COPY Makefile ./
COPY *.h ./
COPY *.c ./
# Ensure no logs
RUN sed -i 's/WITH_LOGS 1/WITH_LOGS 0/' config.h

RUN make release

FROM alpine as runner
COPY --from=builder /lox-c/lox-release /usr/local/bin/lox

CMD ["lox", "repl"]
