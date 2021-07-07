NAME:=dilawars/obs
VERSION:=$(shell date +%Y.%m.%d)

all : build

build : Dockerfile
	docker build -t $(NAME):$(VERSION) .
	docker build -t $(NAME):latest .

upload :
	docker push $(NAME):$(VERSION)
	docker push $(NAME):latest

