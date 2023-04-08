FROM centos:7
MAINTAINER Dilawar Singh <dilawars@ncbs.res.in>

# Install dependencies.
RUN yum -y install epel-release && yum -y update \
      && yum -y clean all --enablerepo='*'
RUN yum install -y git cmake gcc gcc-c++ sudo \
      build-essentials \
      vim gvim python3 python3-setuptools \
      && yum -y clean all --enablerepo='*'

WORKDIR /home/root

RUN git config --global user.name 'Dilawar Singh' && \
      git config --global user.email 'dilawar.s.rajput@gmail.com' && \
      git clone https://github.com/dilawar/vim ~/.vim -b minimal && \
      git clone https://github.com/dilawar/Scripts ~/Scripts --depth 2 
