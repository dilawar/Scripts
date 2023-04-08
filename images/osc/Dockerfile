FROM opensuse/leap:15.4

RUN zypper --non-interactive --quiet addrepo --refresh  \
    https://download.opensuse.org/repositories/devel:languages:rust/15.4/devel:languages:rust.repo
RUN zypper --non-interactive --gpg-auto-import-keys refresh
RUN zypper --non-interactive install \
    --allow-vendor-change \
    osc obs-service-cargo_vendor obs-service-recompress obs-service-tar_scm \
    && zypper clean --all
RUN zypper --non-interactive install \
    --allow-vendor-change \
    build rpmlint \
    && zypper clean --all
WORKDIR /app
