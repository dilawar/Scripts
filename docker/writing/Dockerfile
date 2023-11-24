FROM opensuse/tumbleweed
MAINTAINER Dilawar Singh <dilawars@ncbs.res.in>

ARG PANDOC_VERSION=3.1.8
ARG PANDOC_CROSSREF_VERSION=0.3.17.0

# Install dependencies.
RUN zypper -n install git wget curl && zypper clean
WORKDIR /workdir
RUN cd /tmp \
    && wget https://github.com/jgm/pandoc/releases/download/$PANDOC_VERSION/pandoc-$PANDOC_VERSION-linux-amd64.tar.gz \
    && wget https://github.com/lierdakil/pandoc-crossref/releases/download/v$PANDOC_CROSSREF_VERSION/pandoc-crossref-Linux.tar.xz \
    && tar xvf pandoc-$PANDOC_VERSION-linux-amd64.tar.gz \
    && tar xvf pandoc-crossref-Linux.tar.xz \
    && mv pandoc-crossref /usr/bin/ \
    && ls -ltr pandoc-$PANDOC_VERSION/* \
    && mv pandoc-$PANDOC_VERSION/bin/* /usr/bin/ \
    && rm -rf /tmp/*
RUN zypper -n install ghostscript fontconfig python3 texlive-scheme-small && zypper clean
RUN zypper -n install python3-pygraphviz && zypper clean
RUN python3 -m venv --system-site-packages /root/PY3
ENV PATH="/root/PY3/bin:${PATH}"
RUN echo $PATH
RUN pip install git+https://github.com/dilawar/dilawarpy
RUN pip install pandoc-latex-unlisted
COPY latexmk.pl /usr/bin/latexmk
RUN chmod +x /usr/bin/latexmk && latexmk -v
