FROM opensuse/leap:15.1
MAINTAINER Dilawar Singh <dilawars@ncbs.res.in>

# Install dependencies.
RUN zypper install -y apache2-mod_php7 \
    && rm -rf /var/cache/zypp/packages/*

# php7-imap is not available on 15.1 
RUN zypper addrepo -g https://download.opensuse.org/repositories/home:ecsos:server:Sabre/openSUSE_Leap_15.1/home:ecsos:server:Sabre.repo

RUN zypper --gpg-auto-import-keys install -y --force-resolution \
    php7-imap \
    && rm -rf /var/cache/zypp/packages/*


RUN zypper install -y --force-resolution \
    php7-ldap \
    php7-mbstring \
    php7-pdo \
    php7-mysql \
    php7-imagick \
    php7-json \
    php7-dom \
    && rm -rf /var/cache/zypp/packages/*

RUN zypper install -y \
    tmux \
    && rm -rf /var/cache/zypp/packages/*

RUN a2enmod php7 
RUN a2enmod rewrite
RUN a2enmod ldap
RUN a2enmod imap
RUN a2enmod -l
CMD ["httpd-foreground"]
