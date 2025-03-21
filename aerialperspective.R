# Perspectiva aérea desde mapa de elevaciones con R
# www.overfitting.net
# https://www.overfitting.net/2022/08/perspectiva-aerea-desde-mapa-de.html

library(data.table)  # fread()
library(tiff)


# DEM 1: Sierra de Guadarrama
# Centro de Descargas del Centro Nacional de Información Geográfica
# Modelos de elevaciones en formato raster MDT25
# Cotas en m. Resolución rejilla=25m
# URL: http://centrodedescargas.cnig.es/CentroDescargas/index.jsp

# Leemos y procesamos datos raster
# 4 cuadrantes Sierra Norte de Madrid (Sierra de Guadarrama, Valle del Lozoya
sierra_11=data.matrix(
    fread("PNOA_MDT25_ETRS89_HU30_0483_LID.txt", sep=" ", dec="."))
sierra_12=data.matrix(
    fread("PNOA_MDT25_ETRS89_HU30_0484_LID.txt", sep=" ", dec="."))
sierra_21=data.matrix(
    fread("PNOA_MDT25_ETRS89_HU30_0508_LID.txt", sep=" ", dec="."))
sierra_22=data.matrix(
    fread("PNOA_MDT25_ETRS89_HU30_0509_LID.txt", sep=" ", dec="."))

# Eliminate overlaps and final crop (values obtained manually)
DEM=matrix(0, nrow=1508, ncol=2269)
DEM[1:759, 11:1148]=sierra_11
DEM[14:768, 1136:2269]=sierra_12
DEM[741:1499, 1:1141]=sierra_21
DEM[754:1508, 1129:2265]=sierra_22
DEM=DEM[14:1499, 11:2265]
rm(sierra_11, sierra_12, sierra_21, sierra_22)


# DEM 2: Tenerife
# DEM=readTIFF("tenerife.tif")*3718


# DEM 3: Valle del Lozoya
# DEM=readTIFF("vallelozoya.tif")*2426.159


# DEM 4: sierra de Alicante
# DEM=readTIFF("sierradealicante.tif")*1555.038


# DEM 5: Montserrat
# DEM=readTIFF("montserrat.tif")*1235.732


# DEM 6: Puig Campana
# DEM=readTIFF("puigcampana.tif")*1556.244


dx=25  # dx=2 for Montserrat and Puig Campana  # DEM resolution (m)
DIMX=nrow(DEM)
DIMY=ncol(DEM)
f=1  # Z scale factor

DEM=round(DEM/dx*f)  # round altitudes to a reduced integer set
DEM=DEM-min(DEM)+1  # values>=1, 0 is reserved for no data
MIN=min(DEM)
MAX=max(DEM)
RANGE=MAX-MIN+1

# Display rounded DEM
image(t(DEM[nrow(DEM):1,]), useRaster=TRUE,
      col=c(gray.colors(256, start=0, end=1, gamma=1.8)),
      asp=nrow(DEM)/ncol(DEM), axes=FALSE)


# Build aerial perspective
skyline=array(-1, c(RANGE,DIMY))  # skyline array

# Vectorised main loop
for (val in MIN:MAX) {  # loop through all possible altitudes
    print(paste0("Alt: ", val, "/", RANGE))  # find pixels with altitude=val
    df=as.data.frame(which(DEM==val, arr.ind=TRUE))  # dataframe to aggregate
    colnames(df)=c('X','Y')  # X and Y are DEM coordinates
    df=aggregate(X ~ Y, df, max)  # max=closest dist. to observer for altitude
    skyline[RANGE-val+1, df$Y]=df$X  # store distances to observer
}

# Clean missing (non existent altitude) or wrong (hidden altitude) data
NCOLS=ncol(skyline)
NROWS=nrow(skyline)
for (col in 1:NCOLS) {
    print(paste0("Col: ", col, "/", NCOLS))
    row=1
    while (row < NROWS & skyline[row, col] == -1) row=row+1
    while (row < NROWS & skyline[row, col] != -1) {
        if (skyline[row+1, col] < skyline[row, col])
            skyline[row+1, col] = skyline[row, col]
        row=row+1
    }
}

skyline[skyline==-1]=0  # fill sky with 0=no data (will invert to 1)


# Display and save skyline
skyline=1-skyline/max(skyline)  # normalize and invert

writeTIFF(skyline^(1/1.8),"skyline.tif",
          bits.per.sample=16, compression="LZW")

image(t(skyline[nrow(skyline):1,]), useRaster=TRUE,
      col=c(gray.colors(256, start=0, end=1, gamma=1.8)),
      asp=nrow(skyline)/ncol(skyline), axes=FALSE)
