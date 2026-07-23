OS=centos8-difx
DOCKER_USERNAME=shaoguangleo

all: build release

VERSION='2.9.0a'

build:
	@docker build --tag=$(DOCKER_USERNAME)/$(OS):latest .

release: build
	@docker build --tag=$(DOCKER_USERNAME)/$(OS):$(VERSION) .

push:
	@docker push $(DOCKER_USERNAME)/$(OS):$(VERSION)
	@docker push $(DOCKER_USERNAME)/$(OS):latest