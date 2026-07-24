FROM --platform=linux/amd64 centos:8

# Because Centos8 is EOL, we need to use the repos Vault where old things are archived:
RUN \
    sed -i 's/mirrorlist/#mirrorlist/g' /etc/yum.repos.d/CentOS-* && \
    sed -i 's|#baseurl=http://mirror.centos.org|baseurl=http://vault.centos.org|g' /etc/yum.repos.d/CentOS-*

# Enable epel & power tools repositories
RUN yum clean all && \
    yum -y install epel-release dnf-plugins-core && \
    yum config-manager --set-enabled powertools

RUN yum clean all && yum update -y \
    && yum install -y vim \
    && yum install -y gcc \
    && yum install -y gcc-c++ \
    && yum install -y make \
    && yum install -y libtirpc-devel \
    && yum install -y systemd-devel \
    && yum install -y git \
    && yum install -y libX11-devel\
    && yum install -y gcc-gfortran\
    && yum install -y subversion\
    && yum install -y pkgconfig\
    && yum install -y bison\
    && yum install -y flex\
    && yum install -y expat-devel\
    && yum install -y libtool \
    && yum install -y openmpi\
    && yum install -y wget \
    && yum install -y autoconf-archive \
    && yum install -y python3 \
    && yum install -y gsl gsl-devel \
    && yum install -y fftw fftw-devel

# to avoid source error
#RUN rm /usr/bin/sh && ln -s /usr/bin/bash /usr/bin/sh

RUN tee > /tmp/oneAPI.repo <<EOF
[oneAPI]
name=Intel® oneAPI repository
baseurl=https://yum.repos.intel.com/oneapi
enabled=1
gpgcheck=1
repo_gpgcheck=0
gpgkey=https://yum.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB
EOF

RUN mv /tmp/oneAPI.repo /etc/yum.repos.d/oneAPI.repo

RUN rpm --import https://yum.repos.intel.com/intel-gpg-keys/GPG-PUB-KEY-INTEL-SW-PRODUCTS.PUB

RUN cd ~ \
  && yum install -y --nobest intel-oneapi-ipp-devel \
  && git clone https://github.com/difx/difx \
  && cd difx \
  && git checkout -b release-2.9.0-a origin/release-2.9.0-a \
  && source /root/difx/setup.bash \
  && export IPPROOT=/opt/intel/ \
  && export PATH=$PATH:/usr/lib64/openmpi/bin/ \
  && export MPICXX=/usr/lib64/openmpi/bin/mpicxx \
  && export DIFXROOT=/usr/local/difx \
  && ln -s /usr/bin/python3 /usr/bin/python \
  && python3 /root/difx/genipppc \
  && python3 /root/difx/install-difx
