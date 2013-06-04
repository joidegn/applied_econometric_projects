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
#   first superficial checks indicate that correlation in the first dataset is high, in the second is lower except for X11-X15 where it is very high and in the third it is generally very low (it could be that just due to the sheer number of variables even if the dgps are unrelated, there is a high empirical correlation due to randomness) --> curse of dimensionality maybe
corr1 <- cor(training.data1[, 2:ncol(training.data1)])
corr2 <- cor(training.data2[, 2:ncol(training.data2)])
corr3 <- cor(training.data3[, 2:ncol(training.data3)])

#pairs(training.data2[, 12:16])  # looks like X11-X15 seem to be strongly correlated among each other
# also all the data from ds1.csv seems to be moderately correlated
#   --> this indicates pca can probably be applied to dataset 1 and maybe dataset 2 (TODO: find out why exactly. Something to do with multicollinearity)
#       maybe look at: http://en.wikipedia.org/wiki/Multicollinearity#Detection_of_multicollinearity

#--> correlations from ds1 and ds2 might imply that we should use principal component regression or ridge regression


# principal components:
pr.comp1 <- prcomp(training.data1[, 2:ncol(training.data1)-1], center=T, scale=T)  # we center and scale the variables before pca
# --> it seems the first principal component captures a huge amount of the variation (0.5161)
# this is in line with the correlation findings from above
pr.comp2 <- prcomp(training.data2[, 2:ncol(training.data2)-1], center=T, scale=T)  # we center and scale the variables before pca
# --> again the first principal componen catches a lot but not as much as with first data set (0.1513)
# this may be in line with the correlation findings from above
pr.comp3 <- prcomp(training.data3[, 2:ncol(training.data3)-1], center=T, scale=T)  # we center and scale the variables before pca
# --> pca does not yield very important principal components. correlation of y with all covariates is low as well. I did not look at correlation between the covariates yet.
# and we did not find any high correlations (graphically)



# baseline model
lm.model1 <- lm(y~., data=training.data1) # TODO: I dont know how the baseline models decide which variables are significant
lm.model2 <- lm(y~., data=training.data2) # summary: very high R^2 for models 1 and 2
# test.model2 <- lm(y~X11+X12+X13+X14+X15, data=training.data2) # only one of the five is significant --> the others may be redundant (ANOVA?)
# test.model2.2 <- lm(y~X11, data=training.data2)  # has nearly the same adjusted R^2 as test.model2
# pr.test <- prcomp(training.data2[, 12:16], center=T, scale=T)
# test.model2.3 <- lm(training.data2$y~pr.test$x[,1]) # OLS on first principal component has nearly the same R^2 as OLS on X12-X15
lm.model3 <- lm(y~., data=training.data3)


