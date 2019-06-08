include service_utils/make/utils.mk

DOCKER_ACCOUNT := ftzm
DOCKER_REPO := blog
COMMIT_HASH = $(shell git rev-parse --short=10 HEAD)
DOCKER_BASE_TAG = $(DOCKER_REPO):$(COMMIT_HASH)

make: tag stack-make docker-build

run:
	./scripts/watch.sh

stack-build:
	@echo Making binary
	cd src; stack build

stack-make: stack-build
stack-make:
	@echo Generating static files
	cd src; stack exec sass css/mystyles.scss:css/mystyles.css
	cd src; stack exec ftzm-blog rebuild

docker-build: DOCKER_TAG = $(DOCKER_ACCOUNT)/$(DOCKER_BASE_TAG)$(if $(VERSION),-$(VERSION),)
docker-build:
ifeq ($(shell docker images -q $(DOCKER_TAG) 2>/dev/null),"")
	@echo $(shell docker images -q $(DOCKER_TAG) 2>/dev/null)
	@echo Docker image tagged $(DOCKER_TAG) already exists
else
	-docker rm $(DOCKER_TAG) -f
	docker build -t $(DOCKER_TAG) .
endif
	docker push $(DOCKER_TAG)
