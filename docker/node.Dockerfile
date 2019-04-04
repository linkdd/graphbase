FROM erlang:alpine AS builder

# Install Rebar3
RUN mkdir -p /buildroot/rebar3/bin
ADD https://s3.amazonaws.com/rebar3/rebar3 /buildroot/rebar3/bin/rebar3
RUN chmod a+x /buildroot/rebar3/bin/rebar3

# Setup Environment
ENV PATH=/buildroot/rebar3/bin:$PATH

# Reset working directory
WORKDIR /buildroot

# Copy our Erlang test application
COPY . graphbase

# And build the release
WORKDIR graphbase
RUN rebar3 as prod release

FROM alpine AS runner

# Install some libs
RUN apk add --no-cache openssl && \
    apk add --no-cache ncurses-libs

# Install the released application
COPY --from=builder /buildroot/graphbase/_build/prod/rel/graphbase /graphbase

EXPOSE 7439

CMD ["/graphbase/bin/graphbase", "foreground"]
