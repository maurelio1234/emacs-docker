FROM silex/emacs:27.0-dev

ENV CMAKE=https://cmake.org/files/v3.17/cmake-3.17.0-Linux-x86_64.tar.gz
ENV BOOTSTRAPING=false # true only during bootstraping
ENV DOCKER=true        # you are running on a docker container

# Install base system tools
RUN apt-get update && \
        apt-get install -y \
        wget \
        libtool-bin \
        unzip && \
        wget -qO- "$CMAKE" | tar --strip-components=1 -xz -C /usr/local

RUN mkdir -p ~/Downloads/ && \
        cd ~/Downloads && \
        wget https://www.mdbg.net/chinese/export/cedict/cedict_1_0_ts_utf-8_mdbg.zip && \
        unzip cedict_1_0_ts_utf-8_mdbg.zip

# Copy the files that (almost) never change
COPY ./.emacs.d/bootstrap.el /root/.emacs.d/

# Bootstrap emacs packages
# || true so I can fix things even if the build breaks
RUN BOOTSTRAPING=true \
        emacs -batch --eval "(require 'init \"/root/.emacs.d/bootstrap.el\")" || true

# Now copy the files that change all the time
COPY ./.emacs.d/ /root/.emacs.d/
