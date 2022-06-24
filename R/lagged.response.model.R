#' Using lagged response as a predictor
#' 
#' By default, the model fitted by \code{\link[store]{store}} does not take serial dependence into account.
#' A sinple way to rectify this is to add \eqn{y_{i,j-1}}, the previous value of the response, 
#' as a predictor in the model. While this device models serial dependence, it has 
#' limitations: \enumerate{\item The first observation for each subject must be omitted.
#' \item The resulting model treats the lagged response as continuous,
#' which is at odds with general \code{\link{store}} approach of treating both
#' response and predictors as ordinal. 
#' \item The model assumes a constant effect of the lagged response, 
#' without regard to unequal lags as in, e.g., the \code{thoughts} data set.}
#' The example below illustrates how to include lagged response as a predictor.
#' 
#' @author Prince P. Osei and Philip T. Reiss  
#' %% @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references Osei, P. P. and Reiss, P. T. (2022). Ordinal state-trait regression for intensive longitudinal data. Under revision.
#' @examples
#' \dontrun{
#' 
#' require(dplyr)
#' data(thoughts)
#' thoughts$lagp <- lag(thoughts$pleasant.)
#' thoughts$lagsub <- lag(thoughts$Subject)
#' thoughts$lagday <- lag(thoughts$DAY)
#' summ <- thoughts %>% group_by(Subject, DAY) %>% summarize(medp=median(pleasant., na.rm=TRUE))
#' thoughts <- left_join(thoughts, summ)
#' thoughts$newday <- with(thoughts, lagsub!=Subject | lagday!=DAY)
#' thoughts$newday[1] <- TRUE
#' thoughts$lagp[thoughts$newday==TRUE] <- thoughts$medp[thoughts$newday==TRUE]
#' mod <- store(pleasant.~stressed., data=thoughts, id="Subject")
#' mod_lag <- store(pleasant.~stressed., data=thoughts, id="Subject", covt="lagp")
#' }
#' @name lagged.response.model
NULL


