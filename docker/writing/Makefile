NAME:=dilawars/writing
VERSION:=$(shell date +%Y.%m.%d)

all : build

build : Dockerfile
	podman build -t "$(NAME):latest" .
	podman build -t "$(NAME):$(VERSION)" .

upload :
	podman login docker.io
	podman push $(NAME):$(VERSION)
	podman push $(NAME):latest
