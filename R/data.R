#' Thoughts in time data
#' 
#' This data set consists primarily of five overall thought rating and state measures
#' gathered at each contact in a three-day experience sampling study on ``thoughts in time'',
#' Study 1 of Baumeister et al. (2020). These five measures are based on questions
#' asked at the end of each contact, whose precise wording is given below. In the original data,
#' the five variables were 7-point scales from 0 to 6 (or -3 to +3, for \code{pleasant}). 
#' They have been recoded to range from 1 to 7.
#' 
#' @name thoughts
#' @format A data frame with 6685 rows and the following variables:
#' \describe{
#'      \item{Subject}{Participant ID}
#'      \item{time}{Time of the day}
#'      \item{DAY}{Day on study (1, 2 or 3)}
#'      \item{pleasant}{Altogether, to what extent were your thoughts about something pleasant/unpleasant?
#'      (from very unpleasant to very pleasant)}
#'      \item{absorbed}{How involved are you with what is happening right now?
#'      (from totally detached to totally absorbed)}
#'      \item{satisfied}{How satisfied with your life are you right now?
#'      (from totally detached to totally absorbed)}
#'      \item{exhausted}{How mentally exhausted do you feel right now? 
#'      (from not at all to very much)}
#'      \item{stressed}{How stressed are you right now?
#'      (from not at all to very much)}
#'      \item{pleasant., absorbed., satisfied., exhausted., stressed.}{
#'      Previous five variables, with some merging of categories to avoid small frequencies}
#'      \item{ls}{Life satisfaction (a survey measure taken just once, at the beginning of the study)}
#' }
#' 
#' @references Baumeister, R. F., Hofmann, W., Summerville, A., Reiss, P. T., 
#' Vohs, K. D. (2020). Everyday thoughts in time: Experience sampling studies 
#' of mental time travel. \emph{Personality and Social Psychology Bulletin} 
#' 46(12), 1631--1648.
#' 
#' @examples 
#' # see store
NULL