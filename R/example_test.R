# library(h2o)
# library(dplyr)
# library(ggplot2)
#
# iris_df <- iris %>% mutate(Species = Species == 'setosa')
#
# h2o.init(max_mem_size = '4G')
#
# dataset <- as.h2o(iris_df, destination_frame = 'dataset')
# split <- h2o.splitFrame(dataset, ratios = .75, destination_frames = c('train','test'))
# train <- split[[1]]
# test  <- split[[2]]
#
# drf <- h2o.randomForest(y = 'Species',
#                         training_frame = train,
#                         seed = 1234,
#                         nfolds = 8,
#                         model_id = 'drf')
#
# gbm <- h2o.gbm(y = 'Species',
#                training_frame = train,
#                seed = 1234,
#                nfolds = 8,
#                model_id = 'gbm')
#
# models <- list(RF = drf, GBM = gbm)
#
# h2plots::h2o.plotROC(models, test)
#
#
#

