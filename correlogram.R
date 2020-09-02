merge=read.csv("/Users/avery/Downloads/project1/data/student/merge.csv")

merged<-as.data.frame(merge)


pairs(merged,labels = "Grades", main = "Pairs matrix", pch = 21,
      bg = c("red", "green3", "yellow"), upper.panel = NULL)

library(ggplot2)
library(GGally)
ggpairs(merged, columns=c("G1.y","G2.y","G3.y"), title="grades")

