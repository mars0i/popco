countPropsInDomain <-
function(dom, propnms=c()) {length(grep(paste0("^", dom, "_"), propnms))}
