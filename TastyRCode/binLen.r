# ARK (30-Jul-10) Example for binning by length and explanatory factors.

# Generate a data set.
setA <- trunc( rlnorm( 50, mean=log(400), sd=0.1 ) )
setB <- trunc( rlnorm( 50, mean=log(600), sd=0.1 ) )

fishData <- data.frame( gear=c(rep( "Trawl",length(setA)),rep("Trap",length(setB) )),
                        forkLen=c( setA, setB ) )
                        
cat( "\nfishData dimensions = ",dim(fishData),"\n" )
print( fishData )

# Establish the midpoints, bin size, and the breaks necessary for cut.

binSize <- 50
minSize <- 325
maxSize <- 975
midPoints <- seq( minSize,maxSize,binSize )
cutPoints <- c( midPoints - binSize, max(midPoints)+binSize )

print( midPoints )
print( cutPoints )

# Now eliminate those fish with lengths out of range.
fishData <- fishData[ fishData$forkLen >= min(cutPoints), ]
fishData <- fishData[ fishData$forkLen <= max(cutPoints), ]

cat( "\nfishData dimensions now = ",dim(fishData), "\n" )
print( dim(fishData) )

# Now form a new variable with the length categories.
fishData$lenBin <- cut( fishData$forkLen, breaks=cutPoints )

# Rename the factor "lenBin" variable created by "cut" to character.
# lenBin is an ordered factor, so the levels should be in order, and
# as long as you don't re-order midPoints they will be in the same order.
levels( fishData$lenBin ) <- paste( midPoints )

print( fishData )

# Now table by length category and gear.
lenTable <- table( fishData$gear, fishData$lenBin )
print( lenTable )

# Now normalize the cross-classified table, note this can be extended to
# an arbitrary number of cross-classifying variables.

lenTableNames <- dimnames( lenTable )
result <- expand.grid( lenTableNames, stringsAsFactors=FALSE )
result$counts <- as.vector( lenTable )
names( result ) <- c( "gear","lenBin","count" )

# Now do some sensible sorting.
cat( "\n\nIs this what you want?\n\n" )
result <- result[ order(result$gear), ]
print( result )

