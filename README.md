# listenR
Machine Listening Library for ecoacoustics by Dr.Offig

## Install
ListenR requires some external libraries to be present before installation.
- ffmpeg (http://ffmpeg.org). Actually only libavfilter is needed.
- ImageMagick++ (https://imagemagick.org)
- gdal (https://www.gdal.org)
- Proj.4 (http://download.osgeo.org/proj) 

Once these external dependencies are installed then from an R console type
```code
install.packages("devtools")
devtools::install_github("dr-offig/listenR")
```
