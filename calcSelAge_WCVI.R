# Calc sel at length from WCVI sel_age pars

WCVI_sel <- lisread("WCVI_sel_age.txt")

lenAge <- WCVI_sel$lenAge

for( k in 3:nrow(WCVI_sel$log_sel_age) )
{
  sel_age <- exp(WCVI_sel$log_sel_age[k,2:10])
  
  spline50 <- splinefun(x = lenAge, y = sel_age - .50 )
  spline95 <- splinefun(x = lenAge, y = sel_age - .95 )

  a50 <- uniroot( f= spline50, interval = c(13,25) )$root
  a95 <- uniroot( f= spline95, interval = c(15,30) )$root

  cat(c(a50, a95), "\n")
}