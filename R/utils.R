# utils.R

maxabs <- function(x){ max(abs(x)) }

maxabsdiff <- function(x, y){ maxabs(x - y) }

# like ls(), but more convenient
mls <- function(pat){ ls(globalenv(), pattern=pat) }

butlast <- function(x) head(x, -1)

butfirst <- function(x) tail(x, -1)
