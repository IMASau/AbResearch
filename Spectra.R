## Special issue on chemometrics
## https://www.jstatsoft.org/issue/view/v018


## Article describes an R link into  repository/database of info
## on molecules and fingerprints 
## https://www.jstatsoft.org/article/view/v018i05

setwd('D:/nirs')

library(photobiology)
library(photobiologyInOut)
library(photobiologyWavebands)
library(rOmniDriver)
library(ooacquire)
library(ggspectra)
library(hyperSpec)
library(colorSpec)


update.packages(repos = "http://www.r4photobiology.info/R")

## data in via colorspec
setwd('D:/nirs/Yolk')
temp <- readSpectrumScope("YolkFan_FLMS016771_01_adj.txt")
temp1 <- readSpectrumScope("YolkFan_FLMS016771_10_adj.txt")

plot(temp)
plot(temp1)
summary(temp)                  
wavelength(temp)
is.regular(temp)
step.wl(temp)

temp.smooth <- resample( temp, 200:880, meth='loess', span=0.02 )



class(temp.smooth)
computeCCT(temp)
computeCRI(temp)


## convert to photobiology
## 
testoo <- colorSpec2spct(temp)
testoo1 <- colorSpec2spct(temp1)

summary(testoo)
class(testoo)

rgb_spct(testoo)
color_of(testoo)
s_e_irrad2rgb(testoo)

## Read NIRS data in via ooacquire
setwd('D:/nirs/FLAME')

filename <- "Urchin12_FLMS016771_0002.txt"
oodat <- read_oo_ovdata(filename, time = NULL, geocode = NULL, label = NULL,
                        descriptor = NULL, tz = NULL, locale = NULL,
                        verbose = getOption("photobiology.verbose", default = FALSE))

class(oodat)
plot(oodat)
summary(oodat)

oodat.1 <- as.generic_spct(oodat)
plot(oodat.1)

oodat.2 <- as.source_spct(oodat)

rgb_spct(oodat)
color_of(oodat, type="both")
s_e_irrad2rgb(oodat)


# Plotting using ggspectra
# https://cran.r-project.org/web/packages/ggspectra/vignettes/userguide2-plot-methods.html
plot(testoo)
plot(testoo1)

class(testoo)
plot(testoo, w.band = VIS_bands(), range = VIS())

## Something not quite right here
spct <- rbindspct(list(testoo, testoo1), idfactor = "ID")
plot(spct)

plot(rbindspct(list(urch12 = testoo, urch20 = testoo1)),
     annotations = c("-", "summaries")) + 
 aes(linetype = spct.idx)




plot(testoo) + geom_spct(fill = color_of(testoo)) +
 geom_spct(data=testoo1, color = "black", fill=color_of(testoo1))

two_urchins <- source_mspct(list(urch12 = testoo, urch20 = testoo1))
multiplot(plotlist = mslply(two_urchins, plot))



## Read NIRS data in via hyperspec
setwd('D:/nirs/NQ')
hyp00 <- read.txt.long(
 file = "YolkFan_FLMS016771_01_adj.txt",
 cols = list(.wavelength = "w.length", spc = "s.e.irrad"),
 header = FALSE,
 skip= 17
)

plotspc(hyp00)
qplotspc(hyp00) + aes(colour = spc)
qplotmap(hyp00) + aes(colour = spc)


## photobiology
t1 <- hyperSpec2mspct(hyp00, "source_spct", "s.e.irrad")

t1.1 <- t1[[1]]

class(t1.1)
plot(t1.1)

