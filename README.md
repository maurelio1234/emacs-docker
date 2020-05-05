
# To build locally

``` shell
docker build -t emacs .
```

# Console

``` shell
docker run -it \
    -e "TERM=xterm-256color" \
    -v /home/$USER/.ssh:/root/.ssh \
    -v /mnt/data/bitbucket:/root/bitbucket \
    -v /mnt/data/github:/root/github \
    -v /mnt/data/exercism:/root/exercism \    
    maurelio1234/emacs-docker
```

# GUI

``` shell
docker run -it \
    -e DISPLAY \
    -v /tmp/.X11-unix:/tmp/.X11-unix \
    -v /home/$USER/.ssh:/root/.ssh \
    -v /mnt/data/bitbucket:/root/bitbucket \
    -v /mnt/data/github:/root/github \
    -v /mnt/data/exercism:/root/exercism \
    maurelio1234/emacs-docker 
```
