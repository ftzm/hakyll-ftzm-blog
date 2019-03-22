DOCKER_REPO := blog
COMMIT_HASH = $(shell git rev-parse --short=10 HEAD)
DOCKER_BASE_TAG = $(DOCKER_REPO):$(COMMIT_HASH)

watch:
	./watch.sh

stack-build:
	@echo Making binary
	stack build

stack-make: stack-build
stack-make:
	@echo Generating static files
	stack exec ftzm-blog rebuild

docker-build: DOCKER_TAG = $(DOCKER_BASE_TAG)$(if $(VERSION),-$(VERSION),)
docker-build: stack-make
docker-build:
ifneq ($(shell docker images -q $(DOCKER_TAG) 2>/dev/null),)
	@echo Docker image tagged $(DOCKER_TAG) already exists
else
	docker build -t $(DOCKER_TAG) .
	docker push $(DOCKER_TAG)
endif

make: stack-make docker-build

# ----------------------------------------------------------------------
# Release

SEMVER := ./scripts/semver.sh
COMMIT_RELEASE := ./scripts/commit_release.sh
GIT_TAG_CURRENT = $(shell git describe --exact-match --abbrev=0 2>/dev/null)
GIT_TAG_PREVIOUS = $(or $(shell git describe --abbrev=0 HEAD^ 2>/dev/null), 0.0.0)
GIT_TAG_NEW = $(shell $(SEMVER) -$(or $(SEMVER_FLAG),p) $(GIT_TAG_PREVIOUS))
RELEASE_TYPE = $(shell $(COMMIT_RELEASE))

tag:
ifneq ($(GIT_TAG_CURRENT),)
	@echo Current commit is tagged $(GIT_TAG_CURRENT), skipping
else
	git tag -a $(VERSION) -m "Official release $(VERSION)"
endif

release: VERSION = $(or $(GIT_TAG_CURRENT), $(GIT_TAG_NEW))
release: tag make

patch: RELEASE_TYPE := patch
patch: SEMVER_FLAG := p

minor: RELEASE_TYPE := minor
minor: SEMVER_FLAG := m

major: RELEASE_TYPE := major
major: SEMVER_FLAG := M

major minor patch: release

message:
	@echo $(GIT_COMMIT_MESSAGE)

testreg:
	@echo test
