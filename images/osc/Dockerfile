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
    build rpmlint sudo \
    obs-service-obs_scm obs-service-set_version obs-service-extract_file \
    obs-service-{recompress,download_url,format_spec_file} \
    && zypper clean --all
RUN zypper --non-interactive install \
    --allow-vendor-change \
    obs-service-{tar,cargo_audit} \
    && zypper clean --all
WORKDIR /app
