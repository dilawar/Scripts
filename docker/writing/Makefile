NAME:=dilawars/writing
VERSION:=$(shell date +%Y.%m.%d)

all : build

build : Dockerfile
	docker build -t "$(NAME):latest" .
	docker build -t "$(NAME):$(VERSION)" .

upload :
	docker push $(NAME):$(VERSION)
	docker push $(NAME):latest
