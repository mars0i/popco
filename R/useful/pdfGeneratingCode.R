dfs <- ls() and then remove from it what doesn't belong.
e.g. if the first entry in ls() is something else, you can do
dfs <- ls()[-1]

assumes you've also loaded the main mra, just to get person names:
for (d in dfs){cat(d, "\n"); pdf(file=paste0(sub(".df","",d), "Every40", ".pdf")); for (pers in dimnames(huntingsoc)[[1]]){cat(pers,""); dno <- get(d)[get(d)$dom!="O",]; plot(xyplot(activn~tick|dom, groups=propn, data=dno[dno$person==pers,], ylim=c(-1,1), xlim=c(-10,2010), type="l", main=paste("huntingsociety.lisp", d, "every 40 ticks", pers)))}; dev.off()}

persons <- dimnames(huntingsoc)[[1]]
now you can do it without the main mra:

                                                               change this:                                                                                                                                                                    change this:                         change this:
for (d in dfs){cat("\n", d, "\n"); pdf(file=paste0(sub(".df","",d), "Every100", ".pdf")); for (pers in persons){cat(pers,""); dno <- get(d)[get(d)$dom!="O",]; plot(xyplot(activn~tick|dom, groups=propn, data=dno[dno$person==pers,], ylim=c(-1,1), xlim=c(-10,3010), type="l", main=paste("earthorigindiffuses.lisp", d, "every 100 ticks", pers)))}; dev.off()}
