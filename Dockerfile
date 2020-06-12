FROM silex/emacs:master-dev

# Bring back man pages
# https://github.com/tianon/docker-brew-ubuntu-core/issues/122#issuecomment-380529430
RUN rm /etc/dpkg/dpkg.cfg.d/excludes
# Reinstall all currently installed packages in order to get the man pages back
RUN apt-get update && \
        dpkg -l | grep ^ii | cut -d' ' -f3 | \
        xargs apt-get install -y --reinstall

ENV CMAKE=https://cmake.org/files/v3.17/cmake-3.17.0-Linux-x86_64.tar.gz
# true only during bootstraping
ENV BOOTSTRAPING=false
# you are running on a docker container
ENV DOCKER=true

# Install base system tools
RUN apt-get update && \
        apt-get install -y \
        wget \
        libtool-bin \
        unzip \
        zip \
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
        # https://askubuntu.com/questions/1005623/libdbusmenu-glib-warning-unable-to-get-session-bus-failed-to-execute-child
        dbus-x11 \
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
        python3-pip \
        net-tools \
        socat \
        && \
        wget -qO- "$CMAKE" | tar --strip-components=1 -xz -C /usr/local

RUN locale-gen en_US.UTF-8 || true
ENV LANG=en_US.UTF-8
ENV LANGUAGE=en_US:en
ENV LC_ALL=en_US.UTF-8

# Install Chinese Dictionary
RUN mkdir -p ~/Downloads/cedict_1_0_ts_utf-8_mdbg && \
        cd ~/Downloads/cedict_1_0_ts_utf-8_mdbg && \
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
RUN apt-get install -y \
        dirmngr gnupg apt-transport-https ca-certificates && \
        apt-key adv \
        --keyserver hkp://keyserver.ubuntu.com:80 \
        --recv-keys 3FA7E0328081BFF6A14DA29AA6A19B38D3D831EF && \
        sh -c 'echo "deb https://download.mono-project.com/repo/ubuntu stable-bionic main" > /etc/apt/sources.list.d/mono-official-stable.list' && \
        sudo apt-get update && \
        apt-get install -y mono-complete nuget

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

# Install MS SQL tools
RUN ACCEPT_EULA=y apt-get install -y mssql-cli && \
        apt-get install -f

# Install Web/Node Tools
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

# Add SSL certificates
RUN cert-sync /etc/ssl/certs/ca-certificates.crt \
        && curl https://curl.haxx.se/ca/cacert.pem > ~/cacert.pem \
        && sudo cert-sync ~/cacert.pem

# Man pages
RUN apt-get install -y man manpages manpages-posix manpages-dev

# Helm
RUN curl https://raw.githubusercontent.com/helm/helm/master/scripts/get-helm-3 | bash
# Kubectl
RUN curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl && \
        chmod +x ./kubectl && \
        sudo mv ./kubectl /usr/local/bin/kubectl

# Azure CLI
RUN apt-get install -y ca-certificates curl apt-transport-https lsb-release gnupg
RUN curl -sL https://packages.microsoft.com/keys/microsoft.asc | \
        gpg --dearmor | \
        sudo tee /etc/apt/trusted.gpg.d/microsoft.asc.gpg > /dev/null
RUN AZ_REPO=$(lsb_release -cs) && \
        echo "deb [arch=amd64] https://packages.microsoft.com/repos/azure-cli/ $AZ_REPO main" | \
        sudo tee /etc/apt/sources.list.d/azure-cli.list
RUN apt-get update && apt-get install azure-cli -y

# CircleCI CLI
RUN curl -fLSs https://raw.githubusercontent.com/CircleCI-Public/circleci-cli/master/install.sh | bash

# Define main user

        ARG USER=marcos
RUN addgroup $USER
RUN adduser --disabled-password \
        --shell /bin/bash \
        --ingroup sudo \
        --uid 1000 \
        $USER
RUN echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
RUN addgroup --gid 130 docker
RUN usermod -aG docker $USER

USER $USER
WORKDIR /home/marcos

# Configure git
RUN git config --global user.name "Marcos Almeida" && \
        git config --global user.email marcos.almeida@xcomponent.com && \
        git config --global core.autocrlf input

# Configure Exercism
RUN cd /home/$USER/ && \
        mkdir -p bin && \
        cd bin && \
        wget https://github.com/exercism/cli/releases/download/v3.0.13/exercism-3.0.13-linux-x86_64.tar.gz && \
        tar -xf exercism-3.0.13-linux-x86_64.tar.gz


# Add JetBrains and Noto CJK fonts
RUN mkdir -p ~/.local/share/fonts && \
        wget https://download.jetbrains.com/fonts/JetBrainsMono-1.0.3.zip && \
        unzip JetBrainsMono-1.0.3.zip -d ~/.local/share/fonts && \
        wget "https://github.com/googlei18n/noto-cjk/blob/master/NotoSansCJKsc-Medium.otf?raw=true" --output-document=NotoSansCJKsc-Medium.otf && \
        mv NotoSansCJKsc-Medium.otf ~/.local/share/fonts/ && \
        fc-cache -f -v

# Python Modules for teaching
RUN pip3 install --user pycrypto

# Browsh support
RUN wget https://github.com/browsh-org/browsh/releases/download/v1.6.4/browsh_1.6.4_linux_amd64.deb
RUN sudo apt-get install -y ./browsh_1.6.4_linux_amd64.deb
RUN rm ./browsh_1.6.4_linux_amd64.deb

# Install Firefox too (needed for browsh)
RUN sudo apt-get install -y firefox

# Install Chrome
RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb && \
        sudo apt-get install ./google-chrome-stable_current_amd64.deb -y && \
        rm ./google-chrome-stable_current_amd64.deb

# JQ, very useful
RUN sudo apt-get install -y jq netcat coreutils

# Copy the files that (almost) never change
# TODO find a way to read user name from variable
COPY --chown=marcos:marcos ./.emacs.d/bootstrap.el /home/$USER/.emacs.d/

# Bootstrap emacs packages
RUN BOOTSTRAPING=true \
        emacs -batch \
        --eval "(require 'bootstrap \"/home/$USER/.emacs.d/bootstrap.el\")"

# Now copy the files that change all the time
# TODO find a way to read user name from variable
COPY --chown=marcos:marcos ./.emacs.d/ /home/$USER/.emacs.d/

# Avoid errors like
#        (emacs:1): dbind-WARNING **: 15:08:23.641: Couldn't connect to accessibility bus: Failed to connect to socket /tmp/dbus-7Av9Aisyax: Connection refused
ENV NO_AT_BRIDGE=1
