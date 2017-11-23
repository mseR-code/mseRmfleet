# Test of *.psv file read.
filen <- file( "sableopmod.psv", "rb")
nopar <- readBin(filen, what = integer(), n = 1)
mcmc  <- readBin(filen, what = numeric(), n = nopar * 10000)
mcmc  <- matrix(mcmc, byrow = TRUE, ncol = nopar)