FROM docker.io/opensuse/tumbleweed
RUN zypper -n install php8-cli php8-devel \
    php8-mbstring php8-ffi php8-bz2 php8-intl php8-posix php8-openssl \
    php8-fileinfo \
    php8 cargo openssl-devel && zypper clean --all

# RUN cargo -v
RUN zypper -n install which clang-devel clang php8-xdebug php8 && zypper clean --all
RUN php-config --version
RUN php -v
RUN cargo install cargo-php
WORKDIR /workdir
