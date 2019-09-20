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
#'




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
