source('load_data.R')
library(lars)

# chooses model according to closeness of Cp to p (from above)
lasso.models <- lapply(1:3, function(model.num) {
    working.data <- get(paste0('working.data', model.num))
    return(lars(x=as.matrix(working.data[, 2:ncol(working.data)]), y=working.data$y, type="lasso"))
})

lasso.betas <- lapply(lasso.models, function(lasso.model) {
    cp.diff <- 0:(length(lasso.model$Cp)-1) - lasso.model$Cp  # number of parameters - respective Cp value
    chosen.num <- head(which(cp.diff>0), 1) # and take first element which is positive
    return(lasso.model$beta[chosen.num, ])
})


lar.models <- lapply(1:3, function(model.num) {
    working.data <- get(paste0('working.data', model.num))
    return(lars(x=as.matrix(working.data[, 2:ncol(working.data)]), y=working.data$y, type="lar"))
})

lar.betas <- lapply(lar.models, function(lar.model) {
    cp.diff <- 0:(length(lar.model$Cp)-1) - lar.model$Cp  # number of parameters - respective Cp value
    chosen.num <- head(which(cp.diff>0), 1) # and take first element which is positive
    return(lar.model$beta[chosen.num, ])
})
