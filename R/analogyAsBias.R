
setwd("parenting18analogsAsBiases1i")

csvs <- list.files(pattern="*.csv")
a <- read2multirunRA(csvs)
# This has dimensions person, proposition, tick, run.

#ap <- multiRA2domRA(a, "P")  # and for domain P
#ah <- multiRA2domRA(a, "H")  # extract subarray for proposition domain H
#ap1500 <- ap[1:9,,1500,] # we also strip out SP, the true believer/perceiver, which is person 10
#ah1500 <- ah[1:9,,1500,] # extract subarrays for timestep 1500
ap1500 <- multiRA2domRA(a, "P")[1:9,,1500,] # combine previous steps
ah1500 <- multiRA2domRA(a, "H")[1:9,,1500,] # see preceding notes
# These last matrices have dimensions person, proposition, run.

# Add check here that persons have settled into the same state within each run, 
# except for SP, the true believer/perceiver.  This should be the case.

ap1500m <- apply(ap1500, c(1,3), mean) # for each person and run,
ah1500m <- apply(ah1500, c(1,3), mean) # get average proposition activation
# These matrices have dimensions person, run.

# (Or add the check here.  Within each run, each person's average should be
# almost the same.  But the check above is more careful.)
# Here's one way to check:
sum(apply(ap1500m, c(2), var))
sum(apply(ah1500m, c(2), var))
# Or since the preceding is sensitive to number of persons, you could do:
mean(apply(ap1500m, c(2), var))
mean(apply(ah1500m, c(2), var))
# If the check succeeds, giving small values, then you can just average all
# of the persons in a run together (so maybe combine steps later):

ap1500mm <- apply(ap1500m, c(2), mean) # Since within a run, each person as about the same average activation,
ah1500mm <- apply(ah1500m, c(2), mean) # we might as well just use the average across all persons in each run.
# Note these are vectors, with length equal to the number of runs.

# now combine these into a two-column matrix for use by plot():
both1500mm <- cbind(ah1500mm, ap1500mm)  # the first arg will be x, the second y

plot(both1500mm, type="p", pch="o", ylim=c(-1,1), xlim=c(-1,1))
lines(x=c(-1,1), y=c(-1,1), col="blue") # draw diagonal line indicating where equal P, H pairs would lie
# However, this graph does not give a clear representation of how many points are piled up on each of the shared
# approximate values.
# There are links to some 3-D graphs on the plot man page.
# That will require binning, probably.
# You could try something like this:
# plot(both1500mm, type="p", pch=".", cex=4, ylim=c(-1,1), xlim=c(-1,1))
