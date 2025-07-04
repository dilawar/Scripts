FROM docker.io/opensuse/tumbleweed
MAINTAINER Dilawar Singh <dilawar.s.rajput@gmail.com>

# plantuml 
RUN zypper -n install php8-cli php8-devel \
    php8-mbstring php8-ffi php8-bz2 php8-intl php8-posix php8-openssl \
    php8-fileinfo \
    php8 cargo openssl-devel && zypper clean --all

# RUN cargo -v
# RUN php-config --help
RUN zypper -n install which clang-devel clang php8-xdebug php8 && zypper clean --all
RUN which php
RUN cargo install cargo-php
WORKDIR /workdir
