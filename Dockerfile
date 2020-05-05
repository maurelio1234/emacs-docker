FROM silex/emacs:27.0-dev

ENV CMAKE=https://cmake.org/files/v3.17/cmake-3.17.0-Linux-x86_64.tar.gz
# true only during bootstraping
ENV BOOTSTRAPING=false
# you are running on a docker container
ENV DOCKER=true

# Update
RUN apt-get update

# Install base system tools
RUN apt-get install -y \
        wget \
        libtool-bin \
        unzip \
        locales \
        curl \
        apt-utils \
        ca-certificates \
        gnupg \
        gnupg2 \
        sudo \
        apt-transport-https \
        software-properties-common \
        xvfb \
        x11-xkb-utils \
        xfonts-100dpi \
        xfonts-75dpi \
        xfonts-scalable \
        xfonts-cyrillic \
        x11-apps \
        clang \
        libdbus-1-dev \
        libgtk2.0-dev \
        libnotify-dev \
        libgnome-keyring-dev \
        libgconf2-dev \
        libasound2-dev \
        libcap-dev \
        libcups2-dev \
        libxtst-dev \
        libxss1 \
        libnss3-dev \
        gcc-multilib \
        g++-multilib \
        rabbitmq-server \
        && \
        wget -qO- "$CMAKE" | tar --strip-components=1 -xz -C /usr/local

RUN locale-gen C.UTF-8 || true
ENV LANG=C.UTF-8

# Install Chinese Dictionary
RUN mkdir -p ~/Downloads/ && \
        cd ~/Downloads && \
        wget https://www.mdbg.net/chinese/export/cedict/cedict_1_0_ts_utf-8_mdbg.zip && \
        unzip cedict_1_0_ts_utf-8_mdbg.zip

# Install Docker
RUN set -ex \
    && export DOCKER_VERSION=$(curl --silent --fail --retry 3 https://download.docker.com/linux/static/stable/x86_64/ | grep -o -e 'docker-[.0-9]*-ce\.tgz' | sort -r | head -n 1) \
    && DOCKER_URL="https://download.docker.com/linux/static/stable/x86_64/${DOCKER_VERSION}" \
    && echo Docker URL: $DOCKER_URL \
    && curl --silent --show-error --location --fail --retry 3 --output /tmp/docker.tgz "${DOCKER_URL}" \
    && ls -lha /tmp/docker.tgz \
    && tar -xz -C /tmp -f /tmp/docker.tgz \
    && mv /tmp/docker/* /usr/bin \
    && rm -rf /tmp/docker /tmp/docker.tgz \
    && which docker \
    && (docker version || true)

# Install C# Dev Tools
RUN apt install -y \
        dirmngr gnupg apt-transport-https ca-certificates && \
        apt-key adv \
        --keyserver hkp://keyserver.ubuntu.com:80 \
        --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF && \
        sh -c 'echo "deb https://download.mono-project.com/repo/ubuntu stable-bionic main" > /etc/apt/sources.list.d/mono-official-stable.list' && \
        sudo apt update && \
        apt install -y mono-complete

# Dotnet Core
RUN wget -qO- https://packages.microsoft.com/keys/microsoft.asc | gpg --dearmor > microsoft.asc.gpg
RUN mv microsoft.asc.gpg /etc/apt/trusted.gpg.d/
RUN wget -q https://packages.microsoft.com/config/debian/9/prod.list
RUN mv prod.list /etc/apt/sources.list.d/microsoft-prod.list
RUN chown root:root /etc/apt/trusted.gpg.d/microsoft.asc.gpg
RUN chown root:root /etc/apt/sources.list.d/microsoft-prod.list
RUN apt-get -q update && \
    apt-get install -y \
        dotnet-sdk-3.1

# Install Web/Node Tools
RUN apt-get install -y firefox

RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN curl -sL https://deb.nodesource.com/setup_12.x | bash -

RUN apt-get -q update && \
    apt-get install -y \
    build-essential \
    libsecret-1-dev \
    nodejs \
    yarn

RUN npm install -g gulp-cli
RUN npm install npm@$NPM_VERSION -g

# Copy the files that (almost) never change
COPY ./.emacs.d/bootstrap.el /root/.emacs.d/

# Add SSL certificates
RUN cert-sync /etc/ssl/certs/ca-certificates.crt \
    && curl https://curl.haxx.se/ca/cacert.pem > ~/cacert.pem \
        && sudo cert-sync ~/cacert.pem


# Bootstrap emacs packages
# || true so I can fix things even if the build breaks
RUN BOOTSTRAPING=true \
        emacs -batch --eval "(require 'init \"/root/.emacs.d/bootstrap.el\")" || true

# Now copy the files that change all the time
COPY ./.emacs.d/ /root/.emacs.d/
