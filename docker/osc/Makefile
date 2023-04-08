NAME:=dilawars/obs
VERSION:=$(shell date +%Y.%m.%d)
DOCKER:=$(shell which docker)

all : build

build : Dockerfile
	$(DOCKER) build -t $(NAME):$(VERSION) .
	$(DOCKER) build -t $(NAME):latest .

upload :
	$(DOCKER) push $(NAME):$(VERSION)
	$(DOCKER) push $(NAME):latest

