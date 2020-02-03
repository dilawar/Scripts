FROM centos:7
MAINTAINER Dilawar Singh <dilawars@ncbs.res.in>

# Install dependencies.
RUN yum -y install epel-release && yum -y update \
      && yum -y clean all --enablerepo='*'
RUN yum install -y git cmake gcc gcc-c++ sudo \
      vim gvim python3 python3-setuptools \
      && yum -y clean all --enablerepo='*'
