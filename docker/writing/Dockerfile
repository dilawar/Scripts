FROM docker.io/danteev/texlive
MAINTAINER Dilawar Singh <dilawars@ncbs.res.in>

ENV DEBIAN_FRONTEND=noninteractive
ARG PANDOC_VERSION=3.1.8
ARG PANDOC_CROSSREF_VERSION=0.3.17.0

# Install dependencies.
RUN cd /tmp \
    && wget https://github.com/jgm/pandoc/releases/download/$PANDOC_VERSION/pandoc-$PANDOC_VERSION-linux-amd64.tar.gz \
    && wget https://github.com/lierdakil/pandoc-crossref/releases/download/v$PANDOC_CROSSREF_VERSION/pandoc-crossref-Linux.tar.xz \
    && tar xvf pandoc-$PANDOC_VERSION-linux-amd64.tar.gz \
    && tar xvf pandoc-crossref-Linux.tar.xz \
    && mv pandoc-crossref /usr/bin/ \
    && ls -ltr pandoc-$PANDOC_VERSION/* \
    && mv pandoc-$PANDOC_VERSION/bin/* /usr/bin/ \
    && rm -rf /tmp/*

RUN apt update && apt install -y python3-pygraphviz python3-venv python3-pypandoc && rm -rf /var/lib/apt/lists/*
RUN python3 -m venv --system-site-packages /root/PY3
ENV PATH="/root/PY3/bin:${PATH}"
RUN echo $PATH
RUN pip install git+https://github.com/dilawar/dilawarpy
RUN pip install pandoc-latex-unlisted

# plantuml 
RUN apt update && apt install -y default-jre && rm -rf /var/lib/apt/lists/* 
RUN java --version
RUN cd /root && wget https://github.com/plantuml/plantuml/releases/download/v1.2023.12/plantuml-1.2023.12.jar 
RUN ls /root/*.jar
RUN echo '#!/usr/bin/env bash\nset -e -x\njava -jar /root/plantuml-1.2023.12.jar $@' > /usr/bin/plantuml && chmod +x /usr/bin/plantuml
RUN cat /usr/bin/plantuml
RUN plantuml --version
WORKDIR /workdir
