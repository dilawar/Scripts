FROM opensuse/tumbleweed
MAINTAINER Dilawar Singh <dilawars@ncbs.res.in>

# Install dependencies.
RUN zypper install -y git cmake make \
    vim python3 python3-devel python3-numpy-devel \
    clang \
    && rm -rf /var/cache/zypp/packages/*
WORKDIR /root
RUN git clone --depth 1 https://github.com/dilawar/Scripts
RUN git clone --depth 1 --recursive https://github.com/dilawar/vim ~/.vim
RUN ln -s ~/Scripts/gitconfig ~/.gitconfig
RUN echo "source ~/Scripts/bashrc" >> ~/.bashrc
