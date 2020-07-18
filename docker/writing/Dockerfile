FROM alpine:edge
MAINTAINER Dilawar Singh <dilawars@ncbs.res.in>

# Install dependencies.
WORKDIR /workdir
RUN echo "http://nl.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories && \
    echo "http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
RUN apk update
RUN apk --no-cache -v add wget git vim texlive-luatex texlive-xetex coreutils \
    wget zlib-dev graphviz sed bash
RUN apk --no-cache -v add pandoc python3 && python3 -m ensurepip \
    && rm -rf /usr/lib/python*/ensurepip
RUN cd /tmp \
    && wget https://github.com/jgm/pandoc/releases/download/2.9.2.1/pandoc-2.9.2.1-linux-amd64.tar.gz \
    && wget https://github.com/lierdakil/pandoc-crossref/releases/download/v0.3.6.3/pandoc-crossref-Linux-2.9.2.1.tar.xz \
    && tar xvf pandoc-2.9.2.1-linux-amd64.tar.gz \
    && tar xvf pandoc-crossref-Linux-2.9.2.1.tar.xz \
    && mv pandoc-crossref /usr/bin/ \
    && ls -ltr pandoc-2.9.2.1/* \
    && mv pandoc-2.9.2.1/bin/* /usr/bin/
RUN python3 -m pip install dilawar 
RUN pandoc -v 
