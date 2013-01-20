readcsvs <-
function(csvs) { dfs <- lapply(csvs, readcsv) ; cat("\n"); dfs}
