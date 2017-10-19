cat("I am working hard on loading package..... Please wait\n")
suppressMessages(library('gCMAP'))
 
library('gCMAP')
source('my_wilcox_score.R')

rnorm(50,mean=1,sd=2)->raw_data
data = matrix(raw_data, ncol=2)
raw_data_original = CMAPCollection(data, element='member')
cat("Orignal method result:\n")

print(wilcox_score(raw_data_original[,1], raw_data_original[,2],element="members")$pval)
cat("My result:\n")
print(my_wilcox_score(data[,1],data[,2]))
