#' Plot the ROC Curves
#'
#'
#' @author Elías Alegría <elias.alegria@ug.uchile.cl>
#' @param models list of h2o models class H2OBinomialModel
#' @param newdata dataframe class H2OFrame
#' @param xval if TRUE plot the ROC Curves on cross validation
#'
#' @return ggplot graph
#' @export
#'
#' @seealso h2o.plotLift(), h2o.plotVarImp()
#' @examples
#' # Initialize h2o
#' h2o.init(min_mem_size = '1G', max_mem_size = '4G')
#'
#' # Read the data
#' prostate <- h2o.uploadFile(path = system.file("extdata", "prostate.csv", package = "h2o"),
#'                            destination_frame = "prostate.hex")
#'
#' # Rename target for binomial clasification
#' prostate[,"CAPSULE"] <- h2o.ifelse(prostate[,"CAPSULE"] == 1, 'TRUE', 'FALSE')
#'
#' # Split the data
#' split_h2o <- h2o.splitFrame(prostate, ratios = .7, destination_frames = c('train','test'))
#' train <- split_h2o[[1]]
#' test  <- split_h2o[[2]]
#'
#' # Train models
#' y = "CAPSULE"
#' x = c("AGE", "RACE", "PSA", "VOL", "GLEASON")
#'
#' drf <- h2o.randomForest(y = y, x = x, training_frame = train)
#' glm <- h2o.glm(y = y, x = x, training_frame = train, family = "binomial")
#' gbm <- h2o.gbm(y = y, x = x, training_frame = train)
#'
#' # List of models
#' models <- list(GLM = glm, DRF = drf, GBM = gbm)
#'
#' # Let's Plot ROC Curves
#' h2plots::h2o.plotROC(models, test)
#'
#' # Finish H2O
#' h2o.shutdown()




h2o.plotROC <- function(models, newdata = NULL, xval = FALSE) {

  require(h2o)
  require(dplyr)
  require(ggplot2)

  if (xval & !is.null(newdata)) {
    stop('If the xval argument is TRUE, newdata must bu NULL')
  }
  if (class(newdata) != 'H2OFrame' & !is.null(newdata)) {
    stop('The newdata argument must be class H2OFrame')
  }


  if(is.list(models)) {
    n_models <- length(models)
    data <- NULL

    if (is.null(names(models))) {
      names <- (paste0('model ',1:n_models))
    } else {
      names <- names(models)
    }

    for (i in 1:n_models) {

      # data type validation
      if (class(models[[i]]) != 'H2OBinomialModel') {
        stop('The models list must be class H2OBinomialModel')
      }

      if (xval) {
        performance <- h2o.performance(models[[i]], xval = TRUE)
      } else if (is.null(newdata)) {
        performance <- h2o.performance(models[[i]])
      } else {
        performance <- h2o.performance(models[[i]], newdata = newdata)
      }
      roc_data <- performance@metrics$thresholds_and_metric_scores %>% tbl_df %>% mutate(model = names[i])
      data = bind_rows(data, roc_data)
    }

    g <- ggplot(data, aes(fpr,tpr, color = model)) + geom_line(size=1, alpha = .8)

  } else {

    # data type validation
    if (class(models) != 'H2OBinomialModel') {
      stop('The model must be class H2OBinomialModel')
    }

    performance <- h2o.performance(models, newdata = newdata)
    data <- performance@metrics$thresholds_and_metric_scores %>% tbl_df
    g <- ggplot(data, aes(fpr,tpr)) + geom_line(size=1, alpha = .7, color = '#2655ff')
  }

  g <- g + geom_line(aes(fpr,fpr), data = data, color = 'grey', linetype = 'dashed') +
    ggtitle('ROC Curve') + xlab('False Positive Rate') + ylab('True Positive Rate')

  return(g)
}
