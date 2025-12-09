# Aerial perspective of several places
# www.overfitting.net
# https://www.overfitting.net/2025/12/perspectiva-aerea-desde-mapa-de.html


library(terra)
library(tiff)  # save 16-bit TIFF's
library(Rcpp)


cppFunction('
    NumericMatrix aerial_perspective_cpp(const NumericMatrix& DEM,
                                         double RESOLUTION,
                                         double fscale = 1.0) {
        int NROWS = DEM.nrow();
        int DIMX  = DEM.ncol();
    
        // maximum and final vertical size, scaled by fscale
        double maximo = Rcpp::max(DEM);
        int DIMY = (int) std::round(maximo / RESOLUTION * fscale);
        if (DIMY <= 0) DIMY = 0;
    
        // Precompute integer height matrix H, size NROWS x DIMX
        IntegerMatrix H(NROWS, DIMX);
        for (int j = 0; j < NROWS; ++j) {
            for (int i = 0; i < DIMX; ++i) {
                double v = DEM(j, i) / RESOLUTION * fscale;
                int h = (int) std::round(v);
                if (h < 0) h = 0;
                if (h > DIMY) h = DIMY;
                H(j, i) = h;
            }
        }
    
        // Output directly in the final orientation (equivalent to img[DIMY:1, ])
        NumericMatrix out(DIMY, DIMX); // initialized to 0
    
        // Process each column independently (no parallelism)
        for (int i = 0; i < DIMX; ++i) {
            int assigned = 0; // how many heights (from y=0) have already been assigned
    
            // traverse rows from last to first
            for (int jj = NROWS - 1; jj >= 0 && assigned < DIMY; --jj) {
                int h = H(jj, i);
                if (h <= assigned) continue; // contributes no new height
    
                double fill_value = double(jj + 1) / double(NROWS);
    
                // assign only the new segment: [assigned, h)
                for (int y = assigned; y < h; ++y) {
                    int row = DIMY - 1 - y;  // vertical inversion
                    out(row, i) = fill_value;
                }
    
                assigned = h;
            }
            // the remaining part stays 0 automatically
        }
    
        return out;
    }
')



#################################################

# 1. READ AND PROCESS GEOTIFF DATA

# Read GeoTIFF file. Proyección ETRS89:
# Uso principal: Cartografía y geodesia oficial en Europa (IGN en España, INSPIRE…).
# Origen: Derivado de WGS84/ITRF pero anclado a la placa euroasiática en 1989 (de ahí “89”).
# Objetivo: Tener un sistema estable en Europa que no cambie por el movimiento tectónico.
pirineos=mosaic(rast("MDT02-ETRS89-HU31-0148-3-COB2.tif"),
              rast("MDT02-ETRS89-HU31-0148-4-COB2.tif"),
              rast("MDT02-ETRS89-HU31-0180-1-COB2.tif"),
              rast("MDT02-ETRS89-HU31-0180-2-COB2.tif"),
              rast("MDT02-ETRS89-HU31-0180-3-COB2.tif"),
              rast("MDT02-ETRS89-HU31-0180-4-COB2.tif"),
              fun='mean')
pirineos
plot(pirineos)
RESOLUTION=res(pirineos)[1]  # 2m


# CROP
cropdef=ext(300000, 315000, 4715000, 4728000)
pirineoscrop=crop(x=pirineos, y=cropdef)
pirineoscrop
plot(pirineoscrop)

MAXIMO=as.integer(global(pirineoscrop, "max", na.rm=TRUE))  # 3402m (Aneto)


# Convert to matrix and save as TIFF
DEM=as.matrix(pirineoscrop, wide=TRUE)
defective=which(is.na(DEM))  # fix some NaNs (a whole column of values)
DEM[defective]=(DEM[defective+nrow(DEM)]+DEM[defective-nrow(DEM)])/2
hist(DEM, breaks=800)

DEM=DEM-min(DEM)
MAXIMO=max(DEM)  # new relative max 2049.485m
writeTIFF(DEM/max(DEM), "monteaneto.tif", bits.per.sample=16, compression='LZW')

# Map legend scale
print(paste0("1km over the map of width=", ncol(DEM), " pixels corresponds to ",
             round(1000/RESOLUTION), " pixels"))


#################################################

# 2. GENERATE AERIAL PERSPECTIVE PROFILES


#################################
# 2.1. Find NPROFILES max values and then slice their rows
NPROFILES=5
SEP=10

# 1st round: get ordered y0 max heights
GAP=round(nrow(DEM)/NPROFILES/2)
DEMcopy=DEM
DEMlines=DEM
Ymax=c()
for (n in 1:NPROFILES) {
    print(paste0("Searching for max ", n, "/", NPROFILES))
    ind=which(DEMcopy==max(DEMcopy), arr.ind=TRUE)
    x0=ind[2]
    y0=ind[1]
    Ymax=c(Ymax, y0)
    DEMcopy[(y0-GAP):(y0+GAP),]=0
    DEMlines[(y0-5):(y0+5),]=0
    DEMlines[(y0-20):(y0+20),(x0-20):(x0+20)]=0
}
Ymax=sort(Ymax)
writeTIFF(DEMlines/max(DEMlines), paste0("lines_", NPROFILES, ".tif"),
          bits.per.sample=16)

# 2nd round: get profiles on calculated y0 coords
DIMX=ncol(DEM)
DIMY=ceiling(MAXIMO/RESOLUTION)+SEP*(NPROFILES-1)
img=matrix(0, DIMY, DIMX)
for (n in 1:NPROFILES) {
    print(paste0("Writing profile ", n, "/", NPROFILES))
    values=round(DEM[Ymax[n],]/RESOLUTION+SEP*(NPROFILES-n))
    for (i in 1:DIMX) img[1:values[i], i]=n
}
img=img[DIMY:1,]
writeTIFF(1-img/max(img), paste0("profiles_", NPROFILES, ".tif"),
          bits.per.sample=16)


#################################
# 2.2. Loop and overlap all rows back to front

DIMX=ncol(DEM)
MAXIMO=max(DEM)  # new relative max 2049.485m
DIMY=round(MAXIMO/RESOLUTION)
img=matrix(0, DIMY, DIMX)

NROWS=nrow(DEM)
for (j in 1:NROWS) {
    # print(paste0("Processing row ", j, "/", NROWS))
    values=round(DEM[j,]/RESOLUTION)
    for (i in 1:DIMX) img[1:values[i], i]=j/NROWS
}
img=img[DIMY:1,]

writeTIFF(1-img/max(img), paste0("profilesmonteaneto.tif"),
          bits.per.sample=16)


#################################
# 2.3. Loop and overlap all rows back to front with C++ optimizations (~80 times faster)

img <- aerial_perspective_cpp(DEM, RESOLUTION, fscale=1)

writeTIFF(1-img/max(img), paste0("profilesmonteaneto.tif"),
          bits.per.sample=16)



#################################################

# 3. PICU URRIELLU (NARANJO DE BULNES)


naranjo=mosaic(rast("MDT02-ETRS89-HU30-0056-3-COB2.tif"),
               rast("MDT02-ETRS89-HU30-0055-4-COB2.tif"), fun='mean')
naranjo
plot(naranjo)
RESOLUTION=res(naranjo)[1]  # 2m


# CROP
cropdef=ext(351000, 354000, 4780651, 4788300)
naranjocrop=crop(x=naranjo, y=cropdef)
naranjocrop
plot(naranjocrop)

MAXIMO=as.integer(global(naranjocrop, "max", na.rm=TRUE))  # 2613m


# Convert to matrix and save as TIFF
DEM=as.matrix(naranjocrop, wide=TRUE)
DEM=DEM[nrow(DEM):1, ncol(DEM):1]  # rotation to get view from Bulnes
hist(DEM, breaks=800)

DEM=DEM-min(DEM)
MAXIMO=max(DEM)  # new relative max 1874.9m
writeTIFF(DEM/max(DEM), "picuurriellu.tif", bits.per.sample=16, compression='LZW')

# Map legend scale
print(paste0("1km over the map of width=", ncol(DEM), " pixels corresponds to ",
             round(1000/RESOLUTION), " pixels"))

img <- aerial_perspective_cpp(DEM, RESOLUTION, fscale=1)

writeTIFF(1-img/max(img), paste0("profilespicuurriellu.tif"),
          bits.per.sample=16)



#################################################

# 4.TORROELLA

torroella1=rast("MDT02-ETRS89-HU31-0297-3-COB2.tif")  # resolution 1.99348m
plot(torroella1)
r_new <- rast(ext(torroella1), resolution=2, crs=crs(torroella1))
torroella1 <- resample(torroella1, r_new, method = "bilinear")

torroella2=rast("MDT02-ETRS89-HU31-0296-4-COB2.tif")  # resolution 2m
plot(torroella2)

torroella=mosaic(torroella1, torroella2, fun='mean')
torroella
plot(torroella)
RESOLUTION=res(torroella)[1]  # 2m


# CROP
cropdef=ext(508000, 520000, 4654000, 4658000)
torroellacrop=crop(x=torroella, y=cropdef)
torroellacrop
plot(torroellacrop)

MAXIMO=as.integer(global(torroellacrop, "max", na.rm=TRUE))  # 310m


# Convert to matrix and save as TIFF
DEM=as.matrix(torroellacrop, wide=TRUE)
DEM[DEM<0]=0
hist(DEM, breaks=800)

# From the sea
DEM=t(DEM)
DEM=DEM[,ncol(DEM):1]  # rotation to get view from Bulnes

# From south
DEM=DEM-min(DEM)
MAXIMO=max(DEM)  # 310.233m
writeTIFF(DEM/max(DEM), "torroella.tif", bits.per.sample=16, compression='LZW')
DEM=readTIFF("torroella_rotationmedes.tif")*MAXIMO

# Map legend scale
print(paste0("1km over the map of width=", ncol(DEM), " pixels corresponds to ",
             round(1000/RESOLUTION), " pixels"))

img <- aerial_perspective_cpp(DEM, RESOLUTION, fscale=1)

writeTIFF(1-img/max(img), paste0("profiles_torroella_south_roation.tif"),
          bits.per.sample=16)



#################################################

# 5. ORDESA

ordesa=mosaic(rast("MDT02-ETRS89-HU31-0146-3-COB2.tif"),
              rast("MDT02-ETRS89-HU31-0146-4-COB2.tif"),
              rast("MDT02-ETRS89-HU31-0178-1-COB2.tif"),
              rast("MDT02-ETRS89-HU31-0178-2-COB2.tif"),
              fun='mean')
ordesa
plot(ordesa)
RESOLUTION=res(ordesa)[1]  # 2m

# CROP
cropdef=ext(242000, 260000, 4723000, 4733000)
ordesacrop=crop(x=ordesa, y=cropdef)
ordesacrop
plot(ordesacrop)

MAXIMO=as.integer(global(ordesacrop, "max", na.rm=TRUE))  # 3347m (Monte Perdido)


# Convert to matrix and save as TIFF
DEM=as.matrix(ordesacrop, wide=TRUE)
defective=which(is.na(DEM))  # fix some NaNs (a whole column of values)
DEM[defective]=(DEM[defective+nrow(DEM)]+DEM[defective-nrow(DEM)])/2
hist(DEM, breaks=800)

DEM=DEM-min(DEM)
MAXIMO=max(DEM)  # new relative max 2441.84m
writeTIFF(DEM/max(DEM), "ordesa.tif", bits.per.sample=16, compression='LZW')
# DEM=readTIFF("ordesavalle.tif")*MAXIMO

DEM=DEM-min(DEM)
MAXIMO=max(DEM)  # new relative max 2168.8m

# Map legend scale
print(paste0("1km over the map of width=", ncol(DEM), " pixels corresponds to ",
             round(1000/RESOLUTION), " pixels"))

img <- aerial_perspective_cpp(DEM, RESOLUTION, fscale=1)

writeTIFF(1-img/max(img), paste0("profilesordesa.tif"),
          bits.per.sample=16)



#################################################

# 6. SIERRA NEVADA

sierranevada=mosaic(rast("PNOA_MDT25_ETRS89_HU30_1009_LID.tif"),
              rast("PNOA_MDT25_ETRS89_HU30_1010_LID.tif"),
              rast("PNOA_MDT25_ETRS89_HU30_1011_LID.tif"),
              rast("PNOA_MDT25_ETRS89_HU30_1012_LID.tif"),
              rast("PNOA_MDT25_ETRS89_HU30_1026_LID.tif"),
              rast("PNOA_MDT25_ETRS89_HU30_1027_LID.tif"),
              rast("PNOA_MDT25_ETRS89_HU30_1028_LID.tif"),
              rast("PNOA_MDT25_ETRS89_HU30_1029_LID.tif"),
              rast("PNOA_MDT25_ETRS89_HU30_1041_LID.tif"),
              rast("PNOA_MDT25_ETRS89_HU30_1042_LID.tif"),
              rast("PNOA_MDT25_ETRS89_HU30_1043_LID.tif"),
              rast("PNOA_MDT25_ETRS89_HU30_1044_LID.tif"),
              fun='mean')
sierranevada
plot(sierranevada)
RESOLUTION=res(sierranevada)[1]  # 2m

# CROP
cropdef=ext(440000, 533000, ext(sierranevada)[3], 4130500)
sierranevadacrop=crop(x=sierranevada, y=cropdef)
sierranevadacrop
plot(sierranevadacrop)

MAXIMO=as.integer(global(sierranevadacrop, "max", na.rm=TRUE))  # 3474m (Mulhacén)


# Convert to matrix and save as TIFF
DEM=as.matrix(sierranevadacrop, wide=TRUE)
DEM=DEM[1:(nrow(DEM)-5),]  # crop some NaNs
hist(DEM, breaks=800)

DEM=DEM-min(DEM)
MAXIMO=max(DEM)  # new relative max 3355.75m
writeTIFF(DEM/max(DEM), "sierranevada.tif", bits.per.sample=16, compression='LZW')

# Map legend scale
print(paste0("10km over the map of width=", ncol(DEM), " pixels corresponds to ",
             round(10000/RESOLUTION), " pixels"))

img <- aerial_perspective_cpp(DEM, RESOLUTION, fscale=2)

writeTIFF(1-img/max(img), paste0("profilessierranevada.tif"),
          bits.per.sample=16)
