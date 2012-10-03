# analogsAsBiases2.R
# For processing output from parenting18analogsAsBiases2.lisp and
# associated scripts.

# NOTE: Assumes SP, the true believer/perceiver, is always the first person.

print("loading data into full multirun array...")
csvs <- list.files(pattern="*.csv")
print(system.time(a <- read2multirunRA(csvs)))
# This has dimensions person, proposition, tick, run.

print("subsetting into P/H at 1500 arrays (and removing SP)")
#ap <- multiRA2domRA(a, "P")  # and for domain P
#ah <- multiRA2domRA(a, "H")  # extract subarray for proposition domain H
ap1500 <- multiRA2domRA(a, "P")[2:dim(a)[1],,1500,] # combine previous steps
ah1500 <- multiRA2domRA(a, "H")[2:dim(a)[1],,1500,] # see preceding notes
# NOTE: the "2:dim(a)[1]" says get all rows except the first, which is SP
# These last matrices have dimensions person, proposition, run.

# Add check here that persons have settled into the same state within each run, 
# except for SP, the true believer/perceiver.  This should be the case.

print("making arrays of per person/run average activations")
ap1500m <- apply(ap1500, c(1,3), mean) # for each person and run,
ah1500m <- apply(ah1500, c(1,3), mean) # get average proposition activation
# These matrices have dimensions person, run.

# (Or add the check here.  Within each run, each person's average should be
# almost the same.  But the check above is more careful.)
# Here's one way to check:
#sum(apply(ap1500m, c(2), var))
#sum(apply(ah1500m, c(2), var))
# Or since the preceding is sensitive to number of persons, you could do:

print("average of run variances for P:")
print(mean(apply(ap1500m, c(2), var)))
print("average of run variances for H:")
print(mean(apply(ah1500m, c(2), var)))
# If the check succeeds, giving small values, then you can just average all
# of the persons in a run together (so maybe combine steps later):
print("making per-run average vectors")
ap1500mm <- apply(ap1500m, c(2), mean) # Since within a run, each person as about the same average activation,
ah1500mm <- apply(ah1500m, c(2), mean) # we might as well just use the average across all persons in each run.
# Note these are vectors, with length equal to the number of runs.

print("making 2-column matrix for plotting")
# now combine these into a two-column matrix for use by plot():
both1500mm <- cbind(ah1500mm, ap1500mm)  # the first arg will be x, the second y

print("plotting")
plot(both1500mm, type="p", pch="o", ylim=c(-1,1), xlim=c(-1,1))
lines(x=c(-1,1), y=c(-1,1), col="blue") # draw diagonal line indicating where equal P, H pairs would lie
# However, this graph does not give a clear representation of how many points are piled up on each of the shared
# approximate values.
# There are links to some 3-D graphs on the plot man page.
# That will require binning, probably.
# You could try something like this:
# plot(both1500mm, type="p", pch=".", cex=4, ylim=c(-1,1), xlim=c(-1,1))
