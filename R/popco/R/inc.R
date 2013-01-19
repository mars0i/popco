inc <-
function(i){ eval.parent(substitute(i <- i + 1)); i }
