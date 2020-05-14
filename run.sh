#!/usr/bin/env bash

mode="$1"
image="${2:-maurelio1234/emacs-docker}"
volumes=" -v /home/$USER/.ssh:/home/$USER/.ssh \
          -v /mnt/data/bitbucket:/home/$USER/bitbucket \
          -v /mnt/data/github:/home/$USER/github \
          -v /mnt/data/Downloads:/home/$USER/Downloads \
          -v /mnt/data/github/emacs-docker/.emacs.d/init.el:/home/$USER/.emacs.d/init.el \
          -v /mnt/data/github/emacs-docker/.emacs.d/init-test.el:/home/$USER/.emacs.d/init-test.el \
          -v /mnt/data/github/emacs-docker/.emacs.d/bootstrap.el:/home/$USER/.emacs.d/bootstrap.el \
          -v /mnt/data/exercism:/home/$USER/exercism \
          -v /var/run/docker.sock:/var/run/docker.sock"
common_parametes=" --rm \
                   --net=host \
                   --name emacs"

if [[ "$mode" == "build" ]]; then
    docker build -t "$image" .
elif [[ "$mode" == "text" ]]; then
    docker run $common_parametes \
           --interactive \
           --tty \
           -e "TERM=xterm-256color" \
           $volumes \
           "$image"
elif [[ "$mode" == "gui" ]]; then
    docker run $common_parametes \
           -e DISPLAY \
           -v /tmp/.X11-unix:/tmp/.X11-unix \
           $volumes \
           "$image"
else
    echo "usage: $0 [build|text|gui] [image]"
fi
