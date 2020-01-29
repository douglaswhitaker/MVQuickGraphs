pprint <- function(..., sep="", innersep=" ", outersep1="'", outersep2="'"){
  toPrint <- c()
  for (i in 1:length(list(...))){
    # print a normal object like text (e.g. "testing ")
    if (length(list(...)[[i]]) == 1){
      toPrint <- paste(toPrint,
                       list(...)[[i]],
                       sep = sep)
    }
    # printing a vector (e.g. x <- c(1:10))
    else if (length(list(...)[[i]]) > 1) {
      tempString <- paste(outersep1,
                          list(...)[[i]][1],
                          sep = "")
      for (j in 2:length(list(...)[[i]]))
        tempString <- paste(tempString,
                            list(...)[[i]][j],
                            sep=innersep)
      tempString <- paste(tempString, outersep2, sep = "")
      toPrint <- paste(toPrint, tempString, sep = "")
    }
  }
  print(toPrint)
}

# Is there really an advantage to this over something like
print(paste(c("Testing",1:10,"More testing"),collapse=" "))
pprint("Testing ",1:10," More testing")
pprint("Testing ",1:10," More testing",outersep1="[",outersep2 = "]")

# > pprint(matrix(1:12,ncol=3))
# [1] "'1 2 3 4 5 6 7 8 9 10 11 12'"
# > print(matrix(1:12,ncol=3))
# [,1] [,2] [,3]
# [1,]    1    5    9
# [2,]    2    6   10
# [3,]    3    7   11
# [4,]    4    8   12
# >

# Maybe have pprint be smart about what it prints? That is, detect a matrix and print it on a new line, detect a list and skip over it, etc.?
# Need to have the ability to detect and respond to many different object types

# Another possibility might look like this (desired outcome, not sure quite how to do it yet)
# x <- 5
# pprint("The value was ",x)
# output: The value was x = 5 (identifies the variable name automatically)
#
# A few websites to look at:
# https://stackoverflow.com/questions/39496333/r-get-names-of-arguments-passed-in
# https://stat.ethz.ch/pipermail/r-help/2010-September/252840.html

# Another expansion might be this:
# Specify the character and number of times it should be repeated at the beginning and end of output, e.g.
# pprint("The value was ",x,char="#",char.rep=10, char.beginning = TRUE, char.end = FALSE)
# output:
# ##########
# "The value was 5"
