#!/usr/bin/env bash

mode="$1"
image="${2:-maurelio1234/emacs-docker}"

if [[ "$mode" == "text" ]]; then
    docker run -it \
           -e "TERM=xterm-256color" \
           -v /home/$USER/.ssh:/root/.ssh \
           -v /mnt/data/bitbucket:/home/$USER/bitbucket \
           -v /mnt/data/github:/home/$USER/github \
           -v /mnt/data/exercism:/home/$USER/exercism \
           -v /var/run/docker.sock:/var/run/docker.sock \
           --net=host \
           --name emacs \
           maurelio1234/emacs-docker
elif [[ "$mode" == "build" ]]; then
    docker build -t "$image" .
else
    docker run -it \
           -e DISPLAY \
           -v /tmp/.X11-unix:/tmp/.X11-unix \
           -v /home/$USER/.ssh:/root/.ssh \
           -v /mnt/data/bitbucket:/home/$USER/bitbucket \
           -v /mnt/data/github:/home/$USER/github \
           -v /mnt/data/exercism:/home/$USER/exercism \
           -v /var/run/docker.sock:/var/run/docker.sock \
           --net=host \
           --name emacs \
           "$image"
fi
