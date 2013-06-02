data.set1 <- read.csv('data/ds1.csv')
data.set2 <- read.csv('data/ds2.csv')
data.set3 <- read.csv('data/ds3.csv')

training.data1 <- data.set1[!is.na(data.set1$y),]
training.data2 <- data.set2[!is.na(data.set2$y),]
training.data3 <- data.set3[!is.na(data.set3$y),]

prediction.data1 <- data.set1[is.na(data.set1$y),]
prediction.data2 <- data.set2[is.na(data.set2$y),]
prediction.data3 <- data.set3[is.na(data.set3$y),]

library(ggplot2)
library(gridExtra)

plot.correlograms <- function(data.set, title="") {
    correlograms <- list()
    for (i in 1:(ncol(data.set)-2))  # we leave out last column
        correlograms[[i]] <- ggplot(data=data.set, aes(y=y)) + geom_point(aes_string(x=paste0('X', i+1)))
    correlograms$main <- title
    do.call(grid.arrange, correlograms)
}

#pdf('correlograms.pdf', onefile=T, width=20, height=20)
#plot.correlograms(training.data1, 'training data from ds1')
#plot.correlograms(training.data2, 'training data from ds2')
#plot.correlograms(training.data3, 'training data from ds3')
#dev.off()

# correlation between covariates:
#   first superficial checks indicate: 
pairs(training.data2[, 11:15])  # looks like X11-X15 seem to be strongly correlated among each other
# also all the data from ds1.csv seems to be moderately correlated
#   --> this indicates pca can probably be applied to dataset 1 and maybe dataset 2 (TODO: find out why exactly. Something to do with multicollinearity)
#       maybe look at: http://en.wikipedia.org/wiki/Multicollinearity#Detection_of_multicollinearity

#--> correlations from ds1 and ds2 might imply that we should use principal componen regression or ridge regression

