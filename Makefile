include $(shell nix-shell ../service_utils/shell.nix --run "")

.DEFAULT_GOAL := make

NAME := blog
DOCKER_REPO := ftzm
DOCKER_NAME := $(NAME)

make: tag nix-docker-build

deploy: make docker-publish register

watch:
	nix run -f default.nix generator -c generate-site watch
