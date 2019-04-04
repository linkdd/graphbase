FROM basho/riak-kv:latest

RUN echo "map" >> /etc/riak/schemas/maps.dt
