source('load_data.R')
library(party)


random.forest.models <- lapply(1:3, function(model.num) cforest(y~., data=get(paste0('working.data', model.num)), controls=cforest_unbiased(ntree=1000, mtry=sqrt(ncol(get(paste0('working.data', model.num)))-1))))
# sqrt(num variables) as number of initial preselected predictors from (Strobl 2009))

# this is very slow and not required for prediction but interesting, nonetheless
# system.time(random.forest.variable.importances <- lapply(random.forest.models, function(random.forest.model) varimp(random.forest.model, conditional=T)))
#save(random.forest.variable.importances, file='data/randomForestVariableImportance.RData')
load('data/randomForestVariableImportance.RData')
