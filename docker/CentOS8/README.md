# DiFX Docker Images

To build the Docker image of DiFX, open a terminal in this directory and run:

```bash
$ docker build .
```

And if you want to push the image to Docker Hub, you can run:

```bash
# add a tag to the image
$ make release
# then you can push the image to Docker Hub 
$ make push
```

> Remember to change your Docker Hub username in the Makefile if you want to push the image to Docker Hub.