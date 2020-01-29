source("R/plot4in1.R")
out <- lm(Girth ~ Volume, data = trees)
plot4in1(out)
