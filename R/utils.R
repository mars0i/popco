# utils.R

maxabs <- function(x){ max(abs(x)) }

maxabsdiff <- function(x, y){ maxabs(x - y) }

# like ls(), but more convenient
mls <- function(pat){ ls(globalenv(), pattern=pat) }
