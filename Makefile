DOCKER_REPO := blog
COMMIT_HASH := $(shell git rev-parse --short=10 HEAD)
DOCKER_BASE_TAG := $(DOCKER_REPO):$(COMMIT_HASH)

CURRENT_GIT_TAG := $(git describe --exact-match --abbrev=0 2>/dev/null)
LATEST_GIT_TAG := $(git describe --abbrev=0 2>/dev/null)



RELEASE_DOCKER_TAG =

run:
	./watch.sh

build:
	docker build -t $(DOCKER_BASE_TAG) .

tag:
	@echo $(DOCKER_BASE_TAG)
