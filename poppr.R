library(poppr)

#http://popgen.nescent.org/DifferentiationSNP.html
#https://grunwaldlab.github.io/Population_Genetics_in_R/Getting_ready_to_use_R.html

row.names(loci) <- .genlab("genotype",2451)

loci.poppr <- loci
loci.poppr <- rename(loci.poppr, cmrHr114=cmrHr1.14)
loci.poppr <- rename(loci.poppr, cmrHr230=cmrHr2.30)
loci.poppr <- rename(loci.poppr, Hrub1H08=Hrub1.H08)
loci.poppr <- rename(loci.poppr, Hrub3F03=Hrub3.F03)


obj <- df2genind(loci.poppr, sep=NULL, ploidy=2, ind.names = row.names(loci.poppr), strata=levels, hierarchy = ~Lev1/Lev2/Lev3, type="codom", ncode=6, pop=levels$Lev3)

locus_table(obj)
basic.stats(obj)
wc(obj)
set.seed(20160308) # Setting a seed for a consistent result
grp <- find.clusters(obj, max.n.clust = 10, n.pca = 20, choose.n.clust = FALSE) 
names(grp)
grp$grp

dapc1 <- dapc(obj, grp$grp, n.pca = 20, n.da = 6) 
scatter(dapc1) # plot of the group
scatter(dapc1, scree.da=FALSE)
summary(dapc1)

popNames(obj) # NULL


info_table(obj)

strata(obj)
amova.result <- poppr.amova(obj, hier = ~Lev1/Lev2/Lev3)
amova.result
amova.test <- randtest(amova.result) # Test for significance
plot(amova.test)
amova.test

data(Aeut)
strata(Aeut) <- other(Aeut)$population_hierarchy[-1]
agc <- as.genclone(Aeut)
agc


test <- genind2df(Pinf)

data(monpop)
data(Pinf)
splitStrata(monpop) <- ~Tree/Year/Symptom
setPop(monpop) <- ~Symptom/Year
monpop

amova.result <- poppr.amova(agc, ~Pop/Subpop)
amova.result
amova.test <- randtest(amova.result) # Test for significance
plot(amova.test)
amova.test

# Example with filtering
data(monpop)
splitStrata(monpop) <- ~Tree/Year/Symptom
poppr.amova(monpop, ~Symptom/Year) # gets a warning of zero distances
poppr.amova(monpop, ~Symptom/Year, filter = TRUE, threshold = 0.1) # no warning
