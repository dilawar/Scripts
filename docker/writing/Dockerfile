FROM alpine:edge
MAINTAINER Dilawar Singh <dilawars@ncbs.res.in>

ARG PANDOC_VERSION=2.14.0.1
ARG PANDOC_CROSSREF_VERSION=0.3.11.0a

# Install dependencies.
WORKDIR /workdir
RUN echo "http://nl.alpinelinux.org/alpine/edge/community" >> /etc/apk/repositories && \
    echo "http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories
RUN apk update
RUN apk --no-cache -v add wget git vim coreutils wget zlib-dev graphviz sed bash
RUN apk --no-cache -v add python3 && python3 -m ensurepip \
    && rm -rf /usr/lib/python*/ensurepip
RUN cd /tmp \
    && wget https://github.com/jgm/pandoc/releases/download/$PANDOC_VERSION/pandoc-$PANDOC_VERSION-linux-amd64.tar.gz \
    && wget https://github.com/lierdakil/pandoc-crossref/releases/download/v$PANDOC_CROSSREF_VERSION/pandoc-crossref-Linux.tar.xz \
    && tar xvf pandoc-$PANDOC_VERSION-linux-amd64.tar.gz \
    && tar xvf pandoc-crossref-Linux.tar.xz \
    && mv pandoc-crossref /usr/bin/ \
    && ls -ltr pandoc-$PANDOC_VERSION/* \
    && mv pandoc-$PANDOC_VERSION/bin/* /usr/bin/ \
    && rm -rf /tmp/*
RUN apk --no-cache -v add ghostscript imagemagick
RUN apk --no-cache -v add fontconfig ttf-dejavu perl
RUN python3 -m pip install dilawar pandoc-latex-unlisted
COPY latexmk.pl /usr/bin/latexmk
RUN chmod +x /usr/bin/latexmk && latexmk -v
RUN apk --no-cache -v add texlive-full
