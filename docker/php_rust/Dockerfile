FROM docker.io/opensuse/leap:15.5
MAINTAINER Dilawar Singh <dilawar.s.rajput@gmail.com>

# plantuml 
RUN zypper -n install php8-cli php8-devel cargo openssl-devel && zypper clean --all
# RUN cargo -v
# RUN php-config --help
RUN zypper -n install which clang-devel clang php8-xdebug php8 && zypper clean --all
RUN which php
RUN cargo install cargo-php
WORKDIR /workdir
