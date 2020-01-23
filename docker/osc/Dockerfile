FROM opensuse/tumbleweed
MAINTAINER Dilawar Singh <dilawars@ncbs.res.in>

# Install dependencies.
RUN zypper install -y osc git cmake gcc sudo \
    && rm -rf /var/cache/zypp/packages/*
RUN zypper install -y obs-service-obs_scm \
    obs-service-tar obs-service-tar_scm \
    obs-service-extract_file \
    obs-service-download_url \
    obs-service-download_files \
    obs-service-recompress \
    && rm -rf /var/cache/zypp/packages/*
