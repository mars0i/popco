# utils.R

maxabs <- function(x){ max(abs(x)) }

maxabsdiff <- function(x, y){ maxabs(x - y) }

# like ls(), but more convenient
mls <- function(pat){ ls(globalenv(), pattern=pat) }

butlast <- function(x) head(x, -1)

butfirst <- function(x) tail(x, -1)

#multiseq <- function(froms, tos, by){
#  x <- c(); 
#  for (i in seq_along(froms)){
#    x <- c(x, seq(from=froms[i], to=tos[i], by=by))
#  }
#  x
#}
#
#seqlists <- function(froms, tos, by){
#  mapply
#  x <- c(); 
#  for (i in seq_along(froms)){
#    x <- c(x, seq(from=froms[i], to=tos[i], by=by))
#  }
#  x
#}
