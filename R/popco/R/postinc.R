postinc <-
function(i){ old = i; eval.parent(substitute(i <- i + 1)); old}
