validation.size <- 40
source('load_data.R')  # loads the data and creates validation and training sets
source('evaluation.R')  # to get some comparison functions

library(ggplot2)
library(reshape2)
library(gridExtra)


x.plot <- function(data.set, working.data) {
    plots <- lapply(1:(ncol(working.data)) ggplot(data.frame(y=cbind(working.data$y, data.set$y), type=c(rep('w', nrow(working.data), rep('d', nrow(data.set))))), aes(y=y)))
    plot(data.set3$X2); points(working.data3$X2, col="red")
}
#pdf('correlograms.pdf', onefile=T, width=20, height=20)
#plot.correlograms(training.data1, 'training data from ds1')
#plot.correlograms(training.data2, 'training data from ds2')
#plot.correlograms(training.data3, 'training data from ds3')
#dev.off()

# correlation between covariates:
#   first superficial checks indicate that correlation in the first dataset is quite high, in the second is lower except for X11-X15 where it is very high and in the third it is generally very low (it could be that just due to the sheer number of variables even if the dgps are unrelated, there is a high empirical correlation due to randomness) --> curse of dimensionality maybe
corr1 <- cor(training.data1[, 2:ncol(training.data1)])
corr2 <- cor(training.data2[, 2:ncol(training.data2)])
corr3 <- cor(training.data3[, 2:ncol(training.data3)])

#pairs(training.data2[, 12:16])  # looks like X11-X15 seem to be strongly correlated among each other
# also all the data from ds1.csv seems to be moderately correlated
#   --> this indicates pca can probably be applied to dataset 1 and maybe dataset 2 (TODO: find out why exactly. Something to do with multicollinearity)
#       maybe look at: http://en.wikipedia.org/wiki/Multicollinearity#Detection_of_multicollinearity

#--> correlations from ds1 and ds2 might imply that we should use principal component regression or ridge regression


## baseline models (they are pretty good)
lm.model1 <- lm(y~., data=working.data1) # TODO: I dont know how the baseline models decide which variables are significant
lm.model2 <- lm(y~., data=working.data2) # summary: very high R^2 for models 1 and 2
# test.model2 <- lm(y~X11+X12+X13+X14+X15, data=training.data2) # only one of the five is significant --> the others may be redundant (ANOVA?)
# test.model2.2 <- lm(y~X11, data=training.data2)  # has nearly the same adjusted R^2 as test.model2
# pr.test <- prcomp(training.data2[, 12:16], center=T, scale=T)
# test.model2.3 <- lm(training.data2$y~pr.test$x[,1]) # OLS on first principal component has nearly the same R^2 as OLS on X12-X15
lm.model3 <- lm(y~., data=working.data3)



# best subsets
#library(leaps)
#subset.model1 <- regsubsets(x=training.data1[, 2:ncol(training.data1)], y=training.data1[, 1], really.big=T)
#best.subset.model1<- lm(y~X33, data=training.data1)
#subset.model2 <- regsubsets(x=training.data2[, 2:ncol(training.data2)], y=training.data2[, 1], really.big=T)
#best.subset.model2 <- lm(y~X11, data=training.data2)
#subset.model3 <- regsubsets(x=training.data3[, 2:ncol(training.data3)], y=training.data3[, 1], really.big=T)
#best.subset.model3 <- lm(y~X8 + X38 + X54, data=training.data3)  # chosen according to BIC
### note that best subset selection performs rather badly for model3

#comparison <- compare.mse(best.subset.model, lm.model3, validation.data=validation.data3)
#do.call(grid.arrange, comparison$plots)


# lasso (least angle variant)
library(lars)
lasso.model1 <- lars(as.matrix(training.data1[, -1]), training.data1[, 1])
optimal.step1 <- which(lasso.model1$Cp==min(lasso.model1$Cp)) 
lasso.model1.betas <- lasso.model1$beta[which(lasso.model1$Cp==min(lasso.model1$Cp)), ]
lasso.model1.mse <- mean((predict(lasso.model1, newx=validation.data1[, -1], s=optimal.step1)$fit - validation.data1$y)^2)

lasso.model2 <- lars(as.matrix(training.data2[, -1]), training.data2[, 1])
optimal.step2 <- which(lasso.model2$Cp==min(lasso.model2$Cp)) 
lasso.model2.betas <- lasso.model2$beta[which(lasso.model2$Cp==min(lasso.model2$Cp)), ]
lasso.model2.mse <- mean((predict(lasso.model2, newx=validation.data2[, -1], s=optimal.step2)$fit - validation.data2$y)^2)

lasso.model3 <- lars(as.matrix(training.data3[, -1]), training.data3[, 1])
optimal.step3 <- which(lasso.model3$Cp==min(lasso.model3$Cp)) 
lasso.model3.betas <- lasso.model3$beta[which(lasso.model3$Cp==min(lasso.model3$Cp)), ]
lasso.model3.mse <- mean((predict(lasso.model3, newx=validation.data3[, -1], s=optimal.step2)$fit - validation.data3$y)^2)

### TODO: maybe use diffusion index method (Stock, Watson 2002)

## TODO: forecast combinations, empirical bayes procedures? (Stock and Watson 2006 and references therein)
