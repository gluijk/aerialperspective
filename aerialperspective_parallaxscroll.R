# Aerial perspective with parallax scrolling
# www.overfitting.net
# https://www.overfitting.net/2025/12/perspectiva-aerea-desde-mapa-de.html


library(terra)  # build blur and resample functions
library(tiff)  # save 16-bit TIFF's
library(Rcpp)


cppFunction('
    NumericMatrix aerial_perspective_cpp(const NumericMatrix& DEM,
                                         double RESOLUTION,
                                         double fscale = 1.0,
                                         double maximo = -1.0) {
        int NROWS = DEM.nrow();
        int DIMX  = DEM.ncol();
    
        // Compute maximo if not provided (or provided as negative)
        if (maximo < 0.0) {
            maximo = Rcpp::max(DEM);
        }
    
        // maximum and final vertical size, scaled by fscale
        int DIMY = (int) std::round(maximo / RESOLUTION * fscale);
        if (DIMY <= 0) DIMY = 0;
    
        // Precompute integer height matrix H, size NROWS x DIMX
        IntegerMatrix H(NROWS, DIMX);
        for (int i = 0; i < DIMX; ++i) {
            for (int j = 0; j < NROWS; ++j) {
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
            for (int j = NROWS - 1; j >= 0 && assigned < DIMY; --j) {
                int h = H(j, i);
                if (h <= assigned) continue; // contributes no new height
    
                double fill_value = double(j + 1) / double(NROWS);
    
                // assign only the new segment: [assigned, h)
                for (int y = assigned; y < h; ++y) {
                    int row = DIMY - 1 - y;  // vertical inversion
                    out(row, i) = fill_value;
                }
    
                assigned = h;
            }
        }
    
        return out;
    }
')


cppFunction('
    NumericMatrix build_frame_cpp(NumericMatrix DEMloop,
                                  NumericVector puntero,
                                  int WINDOW)
    {
        int DIMY = DEMloop.nrow();
        int NCOLLOOP = DEMloop.ncol();
        NumericMatrix DEMframe(DIMY, WINDOW);
    
        for(int i = 0; i < DIMY; i++) {
            int start = (int)puntero[i] - 1;
            for(int j = 0; j < WINDOW; j++) {
                int idx = start + j;
                if(idx >= NCOLLOOP) idx %= NCOLLOOP;   // modulus only if needed
                DEMframe(i, j) = DEMloop(i, idx);
            }
        }
        return DEMframe;
    }
')
# C++ implementation of:
# puntero=round(seq(from=1, to=1000, length.out=DIMY))
# 
# NCOLLOOP=ncol(DEMloop)
# DEMframe=matrix(0, nrow=DIMY, ncol=DIMX)  # partial DEM used to build frame
# for (i in 1:DIMY) {
#     idx <- ((puntero[i] - 1 + 0:(WINDOW - 1)) %% NCOLLOOP) + 1  # circular indexing
#     DEMframe[i, ]=DEMloop[i, idx]
# }


#################################################

# 1. READ GEOTIFF DATA

ordesa=mosaic(rast("MDT02-ETRS89-HU31-0146-3-COB2.tif"),
              rast("MDT02-ETRS89-HU31-0146-4-COB2.tif"),
              rast("MDT02-ETRS89-HU31-0178-1-COB2.tif"),
              rast("MDT02-ETRS89-HU31-0178-2-COB2.tif"),
              fun='mean')
ordesa
plot(ordesa)
RESOLUTION=res(ordesa)[1]  # 2m

# CROP
cropdef=ext(239000, 264000, 4724700, 4734000)
ordesacrop=crop(x=ordesa, y=cropdef)
ordesacrop
plot(ordesacrop)

MAXIMO=as.integer(global(ordesacrop, "max", na.rm=TRUE))  # 3347m (Monte Perdido)


# Convert to matrix and save as TIFF
DEM=as.matrix(ordesacrop, wide=TRUE)
defective=which(is.na(DEM))  # fix some NaNs (a whole column of values)
DEM[defective]=(DEM[defective+nrow(DEM)]+DEM[defective-nrow(DEM)])/2
DEM[defective]=(DEM[defective+nrow(DEM)*2]+DEM[defective-nrow(DEM)*2])/2
DEM[defective]=(DEM[defective+nrow(DEM)*3]+DEM[defective-nrow(DEM)*3])/2
DEM[defective]=(DEM[defective+nrow(DEM)*4]+DEM[defective-nrow(DEM)*4])/2
DEM[defective]=(DEM[defective+2]+DEM[defective-2])/2
DEM[defective]=(DEM[defective+2]+DEM[defective-2])/2
hist(DEM, breaks=800)

# DEM=DEM-min(DEM)
DEM=DEM-1800
DEM[DEM<0]=0
MAXIMO=max(DEM)  # new relative max 2441.84m -> 1547.02m
writeTIFF(DEM/max(DEM), "ordesavideo.tif", bits.per.sample=16, compression='LZW')

# Map legend scale
print(paste0("1km over the map of width=", ncol(DEM), " pixels corresponds to ",
             round(1000/RESOLUTION), " pixels"))


#################################################

# 2. BUILD AERIAL PERSPECTIVE

img <- aerial_perspective_cpp(DEM, RESOLUTION, fscale=1)
writeTIFF(1-img, paste0("profilesordesavideo.tif"),
          bits.per.sample=16)


#################################################

# 3. BUILD ENDLESS CYLINDRICAL OVERLAP -> DEMloop


# Build OVERLAP: DEM -> DEMloop
DEMloop=DEM

OVERLAP=2000
j=1
for (x in (ncol(DEM)-OVERLAP+1):ncol(DEM)) {
    alpha=(j-1)/(OVERLAP-1)
    DEMloop[, x]=DEM[, x]*(1-alpha) + DEM[, j]*alpha
    j=j+1
}

DEMloop=DEMloop[, (OVERLAP+1):ncol(DEMloop)]  # drop overlapped area (duplicated)
writeTIFF(DEMloop/MAXIMO, paste0("ordesavideo_loop.tif"),
          bits.per.sample=16)


img <- aerial_perspective_cpp(DEMloop, RESOLUTION, fscale=1)
writeTIFF(1-img, paste0("profilesordesavideo_loop.tif"),
          bits.per.sample=16)


#################################################

# 4. BUILD ANIMATION

# Final frame dimensions (Full HD)
DIMX=1920
DIMY=1080

# Source DEM parameters
NDIMY=nrow(DEMloop)  # depth of DEM in the Y axis (number of depth slices)
WINDOW=DIMX  # number of columns to read on each row
OFFSET=3786  # start at Brecha de Rolando

# Build ANIMATION
inc=seq(from=1, to=10, length.out=NDIMY)
for (n in 0:2500) {
    print(n)
    puntero=1 + round(n*inc) + OFFSET
    DEMframe <- build_frame_cpp(DEMloop, puntero, WINDOW)
    img <- aerial_perspective_cpp(DEMframe, RESOLUTION, fscale=1, maximo=DIMY*RESOLUTION)
    name=paste0("profilesordesavideo_loop_", ifelse(n<10,"000",
                                             ifelse(n<100,"00",
                                             ifelse(n<1000,"0",""))), n, ".tif")
    writeTIFF(1-img, name, bits.per.sample=8)
}

# magick -delay 5 -loop 0 profilesordesavideo_loop_*.tif aerial.gif

# MP4 Video (MPEG-4 AVC/H.264):
# ffmpeg -framerate 24 -i profilesordesavideo_loop_%04d.tif -i racetothemoonclip.wav
#  -c:v libx264 -crf 18 -pix_fmt yuv420p aerial.mp4