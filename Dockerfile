FROM silex/emacs:27.0-dev

COPY ./.emacs.d/ /root/.emacs.d/

ENV CMAKE=https://cmake.org/files/v3.17/cmake-3.17.0-Linux-x86_64.tar.gz
ENV BOOTSTRAPING=false

# Install base system tools
RUN apt-get update && \
        apt-get install -y wget libtool-bin && \
        wget -qO- "$CMAKE" | tar --strip-components=1 -xz -C /usr/local

# Install emacs packages
# || true so I can fix things even if the build breaks
RUN BOOTSTRAPING=true \
        emacs -batch --eval "(require 'init \"/root/.emacs.d/init.el\")" || true
