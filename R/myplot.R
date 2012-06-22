myplot <- function(data){
  cols <- length(data)
  rows <- nrow(data)

  # set up empty plot window with limits xlim, ylim
  plot(1, type="n", ylim=c(-1,1), xlim=c(1,rows))

  for(i in 1:cols){
    lines(data[i], type="l", col=rgb(runif(cols), runif(cols), runif(cols)))
  }
}

# some tips:

# to get data, try something like
# var <- read.csv("mypath")
# var is called a data frame
# Then you can refer to columns by name as var$colname, 
# or refer to cols by index as var[i]
#
# here is something bizarre:
# var[2] refers to the second column
# var[3,2] refers to the third entry in the second column
# var[3,] refers to the third row

# to set/get the directory, use getwd() and setwd(path)
# to load a script, use source(script)


