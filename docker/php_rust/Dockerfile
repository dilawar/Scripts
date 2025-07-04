FROM docker.io/webdevops/php-dev:8.4
MAINTAINER Dilawar Singh <dilawar.s.rajput@gmail.com>

RUN php-config --version
RUN which php

RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | bash -s -- -y
ENV PATH="/root/.cargo/bin:${PATH}"
RUN rustup default stable
RUN apt update && apt install -y clang libclang-dev && apt clean -y
RUN cargo install cargo-php
WORKDIR /workdir
