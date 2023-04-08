#
# dilawars/manylinux
#
FROM quay.io/pypa/manylinux2010_x86_64
MAINTAINER Dilawar Singh <dilawar.s.rajput@gmail.com>

ENV PATH=$PATH:/usr/local/bin
ENV PYDIR39=/opt/python/cp39-cp39/
ENV PATH=$PYDIR39/bin:$PATH

# Read PYPI_PASSWORD  
ARG PYPI_PASSWORD
ENV PYPI_PASSWORD=$PYPI_PASSWORD

RUN yum -y update
RUN yum -y install freeglut-devel libtiff-devel libXmu-devel libXi-devel && yum clean all

RUN python3 -m pip install conan
RUN which conan && conan search poco

RUN git config --global user.name "Dilawar Singh"
RUN git config --global user.email "dilawar@subcom.tech"
WORKDIR /root
RUN ls -ltrh
