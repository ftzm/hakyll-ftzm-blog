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
GIT_TAG_CURRENT = $(shell git describe --exact-match --abbrev=0 2>/dev/null)
GIT_TAG_PREVIOUS = $(or $(shell git describe --abbrev=0 HEAD^ 2>/dev/null), 0.0.0)
GIT_TAG_NEW = $(shell $(SEMVER) -$(or $(SEMVER_FLAG),p) $(GIT_TAG_PREVIOUS))

# Get release type from git commit message
GIT_COMMIT_MESSAGE = $(shell  git --no-pager log --format=%B -n 1)
ifeq ($(findstring release:patch, $(GIT_COMMIT_MESSAGE)))
	RELEASE_TYPE = patch
else ($(findstring release:minor, $(GIT_COMMIT_MESSAGE)))
	RELEASE_TYPE = minor
else ($(findstring release:major, $(GIT_COMMIT_MESSAGE)))
	RELEASE_TYPE = major
endif

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
