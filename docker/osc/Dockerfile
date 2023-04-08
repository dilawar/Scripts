FROM opensuse/tumbleweed
MAINTAINER Dilawar Singh <dilawar.s.rajput@gmail.com>

# Install dependencies.
RUN zypper install -y osc git cmake gcc sudo hostname \
      dpkg-dev \
      && rm -rf /var/cache/zypp/packages/*
RUN zypper install -y obs-service-obs_scm \
    obs-service-tar obs-service-tar_scm \
    obs-service-extract_file \
    obs-service-download_url \
    obs-service-download_files \
    obs-service-recompress \
    rpmdevtools \
    && rm -rf /var/cache/zypp/packages/*
COPY oscrc /root/.config/osc/oscrc
WORKDIR /work
