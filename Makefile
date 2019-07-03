include service_utils/make/utils.mk

DOCKER_REPO := ftzm
DOCKER_NAME := blog

.DEFAULT_GOAL := make

make: tag nix-docker-build

watch:
	nix run -f default.nix generator -c generate-site watch
