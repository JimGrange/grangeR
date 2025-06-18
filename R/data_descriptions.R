#' Sample data from a task switching paradigm by Grange (2025)
#'
#' A data set including data from 51 participants from a task switching study.
#' Experimental manipulations included stimulus set sequence (repetition vs.
#' switch of which stimulus to attend to), response set sequence (repetition
#' vs. switch of what task to perform on the attended stimulus), and
#' compatibility (whether the responses are compatible or incompatible).
#' Response time and accuracy is recorded for each trial.
#'
#' @format A data frame with 24,376 rows and 6 variables:
#' \describe{
#'   \item{id}{participant identification}
#'   \item{response_set_sequence}{whether the current trial is a repeat or a
#'   switch of response set}
#'   \item{stimulus_set_sequence}{whether the current trial is a repeat or a
#'   switch of stimulus set}
#'   \item{compatibility}{whether the current trial's response is compatible
#'   or incompatible}
#'   \item{rt}{response time in milliseconds}
#'   \item{accuracy}{accuracy of the response: 1 for correct, 0 for error}
#' }
#' @references
#'    Grange, J.A. (2025). Control of stimulus set and response set in task
#'    switching. Journal of Experimental Psychology: Learning, Memory, &
#'    Cognition. https://doi.org/10.1037/xlm0001459
"task_switching"




#' Example response time data set from a flanker paradigm.
#'
#' An example data set from an Eriksen flanker task containing multiple
#' participants' data. As well as congruency of the flanker stimuli, the
#' experiment had an additional condition of "warning", which was whether a
#' warning tone was played before stimulus onset or not. Response time and
#' accuracy is recorded for each trial.
#'
#' @format A data frame with 12524 rows and 5 variables:
#' \describe{
#'     \item{subject}{The subject identification number. Note that some
#'           participants were removed due to poor accuracy.}
#'     \item{warning}{The experimental condition (2 in this example). It
#'           logs the presence/absence of an auditory warning tone before
#'           stimulus onset.}
#'     \item{congruency}{The congruency of the flanker stimulus.}
#'     \item{accuracy}{Accuracy of the response; 1 = correct, 0 = error}
#'      \item{rt}{Response time, recorded in seconds.}
#' }
#' #' @references
#'    Grange, J.A. (2016). flankr: An R package for implementing computational
#'    models of attentional selectivity. Behavior Research Methods, 48, 528–541.
#'    https://doi.org/10.3758/s13428-015-0615-y
#'
"flanker_data"
