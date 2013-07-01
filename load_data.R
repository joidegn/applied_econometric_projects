set.seed(20130620)

if (!exists('validation.size'))
    validation.size <- 40
data.set1 <- read.csv('data/ds1.csv')
data.set2 <- read.csv('data/ds2.csv')
data.set3 <- read.csv('data/ds3.csv')
data.set <- list(data.set1, data.set2, data.set3)  # for convenience

working.data1 <- data.set1[!is.na(data.set1$y),]
working.data2 <- data.set2[!is.na(data.set2$y),]
working.data3 <- data.set3[!is.na(data.set3$y),]
working.data <- list(working.data1, working.data2, working.data3)  # for convenience

validation.sample <- sample(1:200, validation.size)
validation.data1 <- working.data1[validation.sample, ]
validation.data2 <- working.data2[validation.sample, ]
validation.data3 <- working.data3[validation.sample, ]
validation.data <- list(validation.data1, validation.data2, validation.data3)  # for convenience

training.data1 <- working.data1[-validation.sample, ]
training.data2 <- working.data2[-validation.sample, ]
training.data3 <- working.data3[-validation.sample, ]
training.data <- list(training.data1, training.data2, training.data3)  # for convenience

prediction.data1 <- data.set1[is.na(data.set1$y),]
prediction.data2 <- data.set2[is.na(data.set2$y),]
prediction.data3 <- data.set3[is.na(data.set3$y),]
prediction.data <- list(prediction.data1, prediction.data2, prediction.data3)  # for convenience

x.vars1 <- paste0('X', 1:(ncol(data.set1)-1))
x.vars2 <- paste0('X', 1:(ncol(data.set2)-1))
x.vars3 <- paste0('X', 1:(ncol(data.set3)-1))


####################################### Convenience functions #######################################
sample.k.folds <- function(data.set, k=3) {
    n <- nrow(data.set)
    fold.size <- floor(n/k)  # we potentially loose some obs here but at least folds are of same size
    return(lapply(1:k, function(sample.num) data.set[sample(n), ][(fold.size*(sample.num-1)+1):(fold.size*sample.num), ]))
}

k.fold.repeat <- function(repeat.num, ...) {  # repeats k.fold repeat.num times. Every result will be in a list. since k.fold returns a list for each fold, we get a list of lists, indexed by first the repetition number and secondly the fold number
    other.args <- list(...)
    lapply(1:repeat.num, function(cur.repetition) do.call(k.fold, other.args))
}

k.fold <- function(data.set, fun, validation.ratio=0.2, seed=NULL, parallel=F, .export=NULL, ...) {  # fun is repeated k times with training.data and validation.data sets being changed every time
    num.folds <- 1/validation.ratio
    if (!is.null(seed)) {
        old.seed <- get(".Random.seed", mode="numeric", envir=globalenv());
        set.seed(seed)
    }
    samples <- sample.k.folds(data.set, num.folds)
    if (!is.null(seed))
        assign(".Random.seed", old.seed, envir=globalenv());
    other.args <- list(...)
    if (parallel==T) {
        for (name in names(other.args)) assign(name, other.args[name])  # dont try this at home, kids!!
        library(foreach)
        foreach(validation.sample.index=1:num.folds, .export=.export) %dopar% {  # needs a registered snow cluster for performance boost
            validation.set <- samples[[validation.sample.index]]
            training.set <- do.call(rbind, samples[-validation.sample.index])
            return(do.call(fun, c(list(training.set, validation.set), other.args)))
        }
    }
    else {
        return(lapply(1:num.folds, function(validation.sample.index) {
            validation.set <- samples[[validation.sample.index]]
            training.set <- do.call(rbind, samples[-validation.sample.index])
            return(do.call(fun, c(list(training.set, validation.set), other.args)))
        }))
    }
}

# just a test function to see if the k.fold function does what it should do with all this environment switching etc. happening due to parallelization
k.fold.test <- function() {
    folds <- k.fold(working.data1, function(training.data, validation.data) {
        return(rbind(training.data, validation.data))
    }, seed=0xbeef)
    folds.parallel <- k.fold(working.data1, function(training.data, validation.data) {
        return(rbind(training.data, validation.data))
    }, parallel=T, seed=0xbeef)
    return(
        all(  # tests if parallel and non parallel are the same for the same seed
            sapply(1:length(folds), function(fold.num) all(folds[[fold.num]] == folds.parallel[[fold.num]]))
        ) && all(# tests if validation set is different every fold for non parallel
            !sapply(1:length(folds), function(fold.num) all(sapply((1:length(folds))[-fold.num], function(other.fold.num) all(folds[[fold.num]] == folds[[other.fold.num]]))))
        ) && all(# tests if validation set is different every fold for parallel
            !sapply(1:length(folds), function(fold.num) all(sapply((1:length(folds.parallel))[-fold.num], function(other.fold.num) all(folds.parallel[[fold.num]] == folds.parallel[[other.fold.num]]))))
        )
    )
}
