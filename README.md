
# To build locally

``` shell
docker build -t emacs .
```

# Console

``` shell
docker run -it \
    -e "TERM=xterm-256color" \
    -v /home/$USER/.ssh:/root/.ssh \
    -v /mnt/data/bitbucket:/home/$USER/bitbucket \
    -v /mnt/data/github:/home/$USER/github \
    -v /mnt/data/exercism:/home/$USER/exercism \
    -v /var/run/docker.sock:/var/run/docker.sock \
    maurelio1234/emacs-docker
```

# GUI

``` shell
docker run -it \
    -e DISPLAY \
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    -v /home/$USER/.ssh:/root/.ssh \
    -v /mnt/data/bitbucket:/home/$USER/bitbucket \
    -v /mnt/data/github:/home/$USER/github \
    -v /mnt/data/exercism:/home/$USER/exercism \
    -v /var/run/docker.sock:/var/run/docker.sock \
    maurelio1234/emacs-docker 
```
