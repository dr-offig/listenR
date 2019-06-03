FROM rocker/geospatial:latest

# miniconda2
#ENV LANG=C.UTF-8 LC_ALL=C.UTF-8
#ENV PATH /opt/conda/bin:$PATH

#RUN apt-get update --fix-missing && \
#    apt-get install -y \
#    	    wget bzip2 ca-certificates curl git && \
#    apt-get clean && \
#    rm -rf /var/lib/apt/lists/*

#RUN wget --quiet https://repo.anaconda.com/miniconda/Miniconda3-4.5.11-Linux-x86_64.sh -O ~/miniconda.sh && \
#    /bin/bash ~/miniconda.sh -b -p /opt/conda && \
#    rm ~/miniconda.sh && \
#    /opt/conda/bin/conda clean -tipsy && \
#    ln -s /opt/conda/etc/profile.d/conda.sh /etc/profile.d/conda.sh && \
#    echo ". /opt/conda/etc/profile.d/conda.sh" >> ~/.bashrc && \
#    echo "conda activate base" >> ~/.bashrc && \
#    source ~/.bashrc && \
#    conda install -c conda-forge -y gdal
    

# install other libraries
RUN apt-get update && apt-get install -y \
    libavfilter-dev \
    libmagick++-dev

# libraries that need to be built from source
#RUN wget https://download.osgeo.org/proj/proj-6.0.0.tar.gz && \
#    gunzip proj-6.0.0.tar.gz && \
#    tar -xvf proj-6.0.0.tar && \
#    cd proj-6.0.0 && \
#    ./configure && make && make install

#RUN wget https://cran.r-project.org/src/contrib/rgdal_1.4-4.tar.gz && \
#    gunzip rgdal_1.4-4.tar.gz && \
#    tar -xvf rgdal_1.4-4.tar


RUN R -e "install.packages('devtools')"
RUN R -e "devtools::install_github('dr-offig/listenR')"
