include service_utils/make/utils.mk

DOCKER_ACCOUNT := ftzm
DOCKER_REPO := blog

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
