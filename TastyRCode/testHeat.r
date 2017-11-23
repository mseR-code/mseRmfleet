# Test heat.colors

testHeat <- function( n=10 )
{
  colVec <- heat.colors( n )
  plot( 1:n,1:n, pch=21, cex=2, bg=rev(colVec) )
}

testHeat( 20 )