NAME:=dilawars/rust_php
VERSION:=$(shell date +%Y.%m.%d)

all : build

build : Dockerfile
	/usr/bin/docker build -t "$(NAME):latest" .
	/usr/bin/docker build -t "$(NAME):$(VERSION)" .

upload :
	/usr/bin/docker login docker.io
	/usr/bin/docker push $(NAME):$(VERSION)
	/usr/bin/docker push $(NAME):latest
