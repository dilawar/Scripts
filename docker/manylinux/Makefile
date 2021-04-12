all : image

LABEL:="dilawars/manylinux:latest"

image : Dockerfile 
	docker build --rm  -t $(LABEL) .

test : image
	docker run -it \
	    --env="DISPLAY" \
	    --volume="/tmp/.X11-unix:/tmp/.X11-unix:rw" \
	    --rm \
	    $(LABEL) bash

push: image
	docker push $(LABEL)
