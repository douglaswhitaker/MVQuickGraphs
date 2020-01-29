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
