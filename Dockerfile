FROM rocker/geospatial:latest

# miniconda2
#ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
#ENV PATH /opt/conda/bin:$PATH

#RUN apt-get update --fix-missing && \
#    apt-get install -y \
#    	    wget bzip2 ca-certificates curl git nasm yasm && \
#    apt-get clean && \
#    rm -rf /var/lib/apt/lists/*


RUN apt-get update -qq && sudo apt-get -y install \
  autoconf \
  automake \
  build-essential \
  cmake \
  nasm \
  yasm \
  git-core \
  libass-dev \
  libfreetype6-dev \
  libsdl2-dev \
  libtool \
  libva-dev \
  libvdpau-dev \
  libvorbis-dev \
  libxcb1-dev \
  libxcb-shm0-dev \
  libxcb-xfixes0-dev \
  libx264-dev \
  libx265-dev \
  libnuma-dev \
  libvpx-dev \
  libopus-dev \
  libmp3lame-dev \
  pkg-config \
  texinfo \
  wget \
  zlib1g-dev


    

# install other libraries
RUN apt-get update && apt-get install -y \
    libavfilter-dev \
    libmagick++-dev

# libraries that are best built from source
RUN git clone https://git.ffmpeg.org/ffmpeg.git ffmpeg && \
    cd ffmpeg && \
    ./configure \
	--enable-gpl \
	--enable-nonfree \
  --enable-libx264 \
  --enable-libx265 \
  --enable-libvpx \
	--pkg-config-flags="--static" \
	--extra-libs="-lpthread -lm" \
	&& make && make install

RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('dr-offig/listenR')"
