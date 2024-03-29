FROM docker-emacs-native-compilation

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

# Install Docker compose
RUN sudo curl -L "https://github.com/docker/compose/releases/download/1.26.2/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose \
        && sudo chmod +x /usr/local/bin/docker-compose

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
        dotnet-sdk-3.1 dotnet-sdk-5.0

# Install MS SQL tools
RUN ACCEPT_EULA=y apt-get install -y mssql-cli && \
        apt-get install -f

# Install Web/Node Tools
RUN curl -sS https://dl.yarnpkg.com/debian/pubkey.gpg | apt-key add -
RUN echo "deb https://dl.yarnpkg.com/debian/ stable main" | tee /etc/apt/sources.list.d/yarn.list
RUN curl -sL https://deb.nodesource.com/setup_17.x | bash -

RUN apt-get -q update && \
        apt-get install -y \
        build-essential \
        libsecret-1-dev \
        nodejs \
        yarn

RUN npm install -g gulp-cli typescript typescript-language-server
RUN npm install npm@$NPM_VERSION -g

# Install Java tools
RUN apt-get install maven -y

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

# Install nvm on my user
RUN curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.1/install.sh | bash

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

# Install terraform cli
RUN wget https://releases.hashicorp.com/terraform/0.12.26/terraform_0.12.26_linux_amd64.zip && \
        unzip terraform_0.12.26_linux_amd64.zip -d /home/$USER/bin/

# AWS CLI
RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" && \
        unzip awscliv2.zip && \
        sudo ./aws/install -i /home/$USER/bin/
# Pandoc
RUN wget https://github.com/jgm/pandoc/releases/download/2.14/pandoc-2.14-linux-amd64.tar.gz && \
    tar xvvf pandoc-2.14-linux-amd64.tar.gz && \
    mv pandoc-2.14 /home/$USER/bin

# Add JetBrains and Noto CJK fonts
RUN mkdir -p ~/.local/share/fonts && \
    wget https://download.jetbrains.com/fonts/JetBrainsMono-1.0.3.zip && \
    unzip JetBrainsMono-1.0.3.zip -d ~/.local/share/fonts && \
    wget "https://github.com/googlefonts/noto-cjk/blob/main/Sans/OTF/SimplifiedChinese/NotoSansCJKsc-Medium.otf?raw=true" --output-document=NotoSansCJKsc-Medium.otf && \
    mv NotoSansCJKsc-Medium.otf ~/.local/share/fonts/ && \
    fc-cache -f -v && \
    rm JetBrainsMono-1.0.3.zip

# Python Modules for teaching
RUN pip3 install --user pycrypto
RUN sudo pip3 install python-language-server

# Install Chrome
RUN wget https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb && \
        sudo apt-get install ./google-chrome-stable_current_amd64.deb -y && \
        rm ./google-chrome-stable_current_amd64.deb

# Install Redis
RUN sudo apt-get install -y redis

# JQ, very useful
RUN sudo apt-get install -y jq netcat coreutils

# Copy the files that (almost) never change
# TODO find a way to read user name from variable
COPY --chown=marcos:marcos ./.emacs.d/bootstrap.el /home/$USER/.emacs.d/

# Install neovim
RUN sudo apt-get install -y libtool autoconf automake cmake libncurses5-dev g++ pkg-config unzip git curl && \
    git clone https://github.com/neovim/neovim.git nvim && \
    cd nvim && \
    make && sudo make install && \
    cd ../ && rm -rf nvim

RUN python3 -m pip install --user --upgrade pynvim

RUN wget https://github.com/junegunn/fzf/releases/download/0.27.2/fzf-0.27.2-linux_amd64.tar.gz && \
    tar -xf fzf-0.27.2-linux_amd64.tar.gz && \
    sudo mv fzf /usr/local/bin

RUN wget https://github.com/sharkdp/fd/releases/download/v8.2.1/fd_8.2.1_amd64.deb && \
    sudo dpkg -i fd_8.2.1_amd64.deb

RUN wget https://github.com/sharkdp/bat/releases/download/v0.18.3/bat_0.18.3_amd64.deb && \
    sudo dpkg -i bat_0.18.3_amd64.deb

RUN curl -LO https://github.com/BurntSushi/ripgrep/releases/download/12.1.1/ripgrep_12.1.1_amd64.deb && \
    sudo dpkg -i ripgrep_12.1.1_amd64.deb

# Install Plug
RUN sh -c 'curl -fLo "${XDG_DATA_HOME:-$HOME/.local/share}"/nvim/site/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'

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

# So I can run puppeteer tests locally
ENV PUPPETEER_CHROMIUM_PATH=/usr/bin/google-chrome
