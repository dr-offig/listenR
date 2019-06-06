FROM rocker/geospatial:latest

# install other libraries
RUN apt-get update && apt-get install -y \
    libavfilter-dev \
    libmagick++-dev

RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('dr-offig/listenR')"
