foci2intervals <-
function(foci){ c(-1, (foci - (foci[2]-foci[1])/2)[-1], 1) }
