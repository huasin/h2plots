#' Plot the Lift Curve of one or more model
#'
#'
#' @author Elías Alegría <elias.alegria@ug.uchile.cl>
#' @param models list of h2o models class H2OBinomialModel
#' @param newdata dataframe class H2OFrame
#' @param xval if TRUE plot the Lift Curves on cross validation
#'
#' @return ggplot graph
#' @export
#'
#' @seealso h2o.plotROC(), h2o.plotVarImp()
#' @examples
#'


h2o.plotLift <- function(models, newdata = NULL, xval = FALSE, groups = 100) {

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
    # n_models <- length(models)
    # data <- NULL
    #
    # if (is.null(names(models))) {
    #   names <- (paste0('model ',1:n_models))
    # } else {
    #   names <- names(models)
    # }
    #
    # for (i in 1:n_models) {
    #
    #   # data type validation
    #   if (class(models[[i]]) != 'H2OBinomialModel') {
    #     stop('The models list must be class H2OBinomialModel')
    #   }
    #
    #   if (xval) {
    #     performance <- h2o.performance(models[[i]], xval = TRUE)
    #   } else if (is.null(newdata)) {
    #     performance <- h2o.performance(models[[i]])
    #   } else {
    #     performance <- h2o.performance(models[[i]], newdata = newdata)
    #   }
    #   roc_data <- performance@metrics$thresholds_and_metric_scores %>% tbl_df %>% mutate(model = names[i])
    #   data = bind_rows(data, roc_data)
    # }
    #
    # g <- ggplot(data, aes(fpr,tpr, color = model)) + geom_line(size=1, alpha = .8)

  } else {

    # data type validation
    if (class(models) != 'H2OBinomialModel') {
      stop('The model must be class H2OBinomialModel')
    }

    # GET THE DATA
    scores <- h2o.predict(models, newdata) %>%
      tbl_df() %>%
      select(3) %>%
      as.matrix %>%
      as.vector

    # name of the response var
    response_var <- models@parameters$y

    # get the response vector from the newdata
    response <- newdata %>%
      tbl_df %>%
      select(response_var) %>%
      as.matrix %>%
      as.vector

    # create the dataset
    dataset <- tibble(score = scores,
                      response = response)

    # mean rate
    tasa_promedio = mean(as.logical(dataset$response))

    # get the lift dataset
    data_lift <- dataset %>%
      arrange(-score) %>%
      mutate(percentil = cut(rank(score,ties.method = "first"),
                             quantile(rank(score,ties.method = "first"),seq(from = 0, to = 1, length.out = groups + 1)),
                             include.lowest = T,
                             labels = seq(from = 100, to = 1, length.out = groups))) %>%
      group_by(percentil) %>%
      summarise(min_score = min(score),
                max_score = max(score),
                mean_score = mean(score),
                response_rate = mean(as.logical(response)),
                detected = sum(as.logical(response)),
                feqcuenty = n()) %>%
      ungroup() %>%
      mutate(lift = response_rate/tasa_promedio,
             percentil = as.numeric(as.character(percentil))) %>%
      arrange(percentil) %>%
      mutate(cum_response = cumsum(detected)/cumsum(feqcuenty),
             cum_lift = cum_response/tasa_promedio)

    g <- ggplot(data_lift, aes(percentil,cum_lift)) + geom_line(size=1, alpha = .7, color = '#2655ff')
  }

  random <- tibble(x = 1:groups,
                   y = 1)

  g <- g + geom_line(aes(x,y), data = random, color = 'grey', linetype = 'dashed') +
    ggtitle('Lift Curve') + xlab('Percentile') + ylab('Cumulative Lift')

  return(g)
}
