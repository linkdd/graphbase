STACK_NAME = graphbase

DOCKER_COMPOSE_BIN = docker-compose
DOCKER_COMPOSE_FILE = docker/docker-compose.yml
DOCKER_COMPOSE = $(DOCKER_COMPOSE_BIN) -f $(DOCKER_COMPOSE_FILE) -p $(STACK_NAME)

.PHONY: all
all:
	@echo "Commands:"
	@echo "  build          Build all containers"
	@echo "    build/kvs    Build the Key-Value Store container"
	@echo "    build/node   Build the Graphbase Node container"
	@echo "  up             Start all containers"
	@echo "    up/kvs       Start the Key-Value Store container"
	@echo "    up/node      Start the Graphbase Node container"
	@echo "  stop           Stop all containers"
	@echo "    stop/kvs     Stop the Key-Value Store container"
	@echo "    stop/node    Stop the Graphbase Node container"
	@echo "  rm             Remove all containers"
	@echo "    rm/kvs       Remove the Key-Value Store container"
	@echo "    rm/node      Remove the Graphbase Node container"

.PHONY: build
build:
	$(DOCKER_COMPOSE) build

.PHONY: build/kvs
build/kvs:
	$(DOCKER_COMPOSE) build kvs

.PHONY: build/node
build/node:
	$(DOCKER_COMPOSE) build node

.PHONY: up
up:
	$(DOCKER_COMPOSE) up

.PHONY: up/kvs
up/kvs:
	$(DOCKER_COMPOSE) up kvs

.PHONY: up/node
up/node:
	$(DOCKER_COMPOSE) up node

.PHONY: stop
stop:
	$(DOCKER_COMPOSE) stop

.PHONY: stop/kvs
stop/kvs:
	$(DOCKER_COMPOSE) stop kvs

.PHONY: stop/node
stop/node:
	$(DOCKER_COMPOSE) stop node

.PHONY: rm
rm:
	$(DOCKER_COMPOSE) rm

.PHONY: rm/kvs
rm/kvs:
	$(DOCKER_COMPOSE) rm kvs

.PHONY: rm/node
rm/node:
	$(DOCKER_COMPOSE) rm node