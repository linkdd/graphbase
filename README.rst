graphbase
=========

Graphbase is a distributed graph oriented database built on top of a the Riak_ distributed Key-Value store.

Each node of the cluster expose an HTTP API providing the following features:

 - external authentication through the ``REMOTE_USER`` HTTP header whose value maps to a Graphbase user name
 - Prometheus_ metrics exporter, exposing metrics from the whole cluster
 - POST endpoint allowing to send requests using the Graphbase DSL

Many of the graph related algorithms are done using the Map-Reduce framework emapred_.

See the documentation_ for more informations.

.. _Riak: https://docs.riak.com/riak/kv/latest/index.html
.. _Prometheus: https://prometheus.io/
.. _emapred: https://github.com/linkdd/emapred.git
.. _documentation: https://github.com/linkdd/graphbase/wiki

Using Docker
------------

A ``Makefile`` is present to pilot ``docker-compose``

.. code-block:: console

   Print help
   $ make

   Build the Riak based Key-Value store image
   $ make build/kvs
   
   Build the Graphbase Node image
   $ make build/node
   
   Start the cluster
   $ make up
