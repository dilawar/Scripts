IMAGENAME := subcomdocker/osc

all : Dockerfile
	docker build -t $(IMAGENAME) .

upload:
	docker push $(IMAGENAME)

.PHONY: all upload
