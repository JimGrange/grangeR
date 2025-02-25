#' Compute Conditional Distribution Functions (CDF) from Response Time Data
#'
#' This function computes CDF values from response time data, optionally
#' grouping by conditions.
#' @param data A data frame / tibble in long format with columns containing
#' (at the very least) response time and accuracy.
#'
#' @param rt_var The column name where response times can be found.
#'
#' @param accuracy_var The column name where accuracy can be found.
#'
#' @param id_var If the data contains responses from multiple participants, the
#' column name where the participant id can be found.
#'
#' @param conditions If the data contains responses from multiple conditions,
#' list the column names where the levels of the condition are coded.
#' If there are multiple conditions, provide a concatenated vector of condition
#' column names (e.g., c("age_group", "gender")).
#'
#' Note that for plotting, only three conditions can be plotted. The order of
#' conditions matters when there are more than two conditions:
#' * If there are just two conditions,the first condition will be grouped on the
#' plot and the second condition will be faceted as columns.
#' * If there are three conditions, the first condition
#' will be grouped, the second will be faceted as columns, and the third will
#' be faceted as rows.
#'
#' @param include_errors A logical variable (TRUE/FALSE) indicating whether the
#' CDF calculations should include error trials or not.
#'
#' @param probs A vector of probability values for the CDF.
#'
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom ggplot2 ggplot aes coord_flip element_text geom_errorbar
#' geom_point geom_line theme theme_bw labs scale_colour_discrete
#' scale_x_continuous facet_grid
#' @importFrom rlang .data
#' @importFrom stats sd quantile
#' @importFrom tidyr unnest_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom stringr str_remove
#'
#' @export

get_cdf <- function(data,
                    rt_var = "rt",
                    accuracy_var = "accuracy",
                    id_var = NULL,
                    conditions = NULL,
                    include_errors = FALSE,
                    probs = c(.1, .3, .5, .7, .9)){


  # if id is null (i.e., perhaps it's data from just a single participant or
  # a model), add an id column
  if(is.null(id_var)){
    data <- data |>
      mutate(id = 1)
  }

  # if id is not null, add column called id
  if(!is.null(id_var)){
    if(id_var != "id"){
      data$id <- data[[id_var]]
    }
  }

  # remove errors if requested
  if(include_errors == FALSE){
    data <- data |>
      filter(data[[accuracy_var]] == 1)
  }

 # get the CDF values for each individual
 id_data <- data |>
   group_by(.data$id,
            across(all_of(conditions))) |>
   summarise(quantiles = list(quantile(.data[[rt_var]],
                                       probs = probs)),
             .groups = "drop") |>
   unnest_wider(.data$quantiles) |>
   rename_with(~ str_remove(.x, "%")) |>
   pivot_longer(cols = c(-conditions, -.data$id),
                names_to = "quantile",
                values_to = rt_var) |>
   mutate(quantile = as.numeric(quantile) / 100)

 # get the CDF data averaged across subjects
 if(is.null(id_var)){
   id_averaged_data <- id_data |>
     rename(mean_rt = .data$rt) |>
     mutate(se_rt = 0)
 } else {
   id_averaged_data <- id_data |>
     group_by(across(all_of(conditions)),
              .data$quantile) |>
     summarise(mean_rt = mean(.data$rt),
               se_rt = sd(.data$rt) / sqrt(length(.data$rt)),
               .groups = "drop")
 }

 if(id_var != "id"){
   data <- data |>
     select(-.data$id)
 }


 #--- do the plotting

 # plot if there are no conditions
 if(is.null(conditions)){
   plot <- id_averaged_data |>
     ggplot(aes(x = .data$quantile,
                y = .data$mean_rt)) +
     geom_point(size = 3) +
     geom_line(linewidth = 1) +
     geom_errorbar(aes(ymin = .data$mean_rt - .data$se_rt,
                       ymax = .data$mean_rt + .data$se_rt),
                   width = 0.02) +
     theme_bw() +
     labs(y = "Mean Response Time (ms)",
          x = "Probability") +
     scale_x_continuous(breaks = probs,
                        limits = c(0, 1)) +
     coord_flip() +
     theme(text = element_text(size = 18)) +
     theme(legend.position = "bottom")
 }

 # plot if there is one condition
 if(length(conditions) == 1){
   id_averaged_data$condition <- as.factor(id_averaged_data[[conditions]])
   plot <- id_averaged_data |>
     ggplot(aes(x = .data$quantile,
                y = .data$mean_rt,
                group = .data$condition)) +
     geom_point(aes(colour = .data$condition),
                size = 3) +
     geom_line(aes(colour = .data$condition),
               linewidth = 1) +
     geom_errorbar(aes(ymin = .data$mean_rt - .data$se_rt,
                       ymax = .data$mean_rt + .data$se_rt,
                       colour = .data$condition),
                   width = 0.02) +
     theme_bw() +
     labs(y = "Mean Response Time (ms)",
          x = "Probability") +
     scale_x_continuous(breaks = probs,
                        limits = c(0, 1)) +
     coord_flip() +
     scale_colour_discrete(name = conditions) +
     theme(text = element_text(size = 18)) +
     theme(legend.position = "bottom")

   # remove the conditions tag
   id_averaged_data <- id_averaged_data |> select(-.data$condition)
 }

 # plot if there are two conditions
 if(length(conditions) == 2){
   id_averaged_data$condition_1 <- as.factor(id_averaged_data[[conditions[1]]])
   id_averaged_data$condition_2 <- as.factor(id_averaged_data[[conditions[2]]])
   plot <- id_averaged_data |>
     ggplot(aes(x = .data$quantile,
                y = .data$mean_rt,
                group = .data$condition_1)) +
     geom_point(aes(colour = .data$condition_1),
                size = 3) +
     geom_line(aes(colour = .data$condition_1),
               linewidth = 1) +
     geom_errorbar(aes(ymin = .data$mean_rt - .data$se_rt,
                       ymax = .data$mean_rt + .data$se_rt,
                       colour = .data$condition_1),
                   width = 0.02) +
     facet_grid(.~.data$condition_2) +
     theme_bw() +
     labs(y = "Mean Response Time (ms)",
          x = "Probability") +
     scale_x_continuous(breaks = probs,
                        limits = c(0, 1)) +
     coord_flip() +
     scale_colour_discrete(name = conditions[1]) +
     theme(text = element_text(size = 18)) +
     theme(legend.position = "bottom")

   # remove the conditions tag
   id_averaged_data <- id_averaged_data |> select(-.data$condition_1,
                                                  -.data$condition_2)
 }

 # plot if there are three conditions
 if(length(conditions) == 3){

   id_averaged_data$condition_1 <- as.factor(id_averaged_data[[conditions[1]]])
   id_averaged_data$condition_2 <- as.factor(id_averaged_data[[conditions[2]]])
   id_averaged_data$condition_3 <- as.factor(id_averaged_data[[conditions[3]]])

   plot <- id_averaged_data |>
     ggplot(aes(x = .data$quantile,
                y = .data$mean_rt,
                group = .data$condition_1)) +
     geom_point(aes(colour = .data$condition_1),
                size = 3) +
     geom_line(aes(colour = .data$condition_1),
               linewidth = 1) +
     geom_errorbar(aes(ymin = .data$mean_rt - .data$se_rt,
                       ymax = .data$mean_rt + .data$se_rt,
                       colour = .data$condition_1),
                   width = 0.02) +
     facet_grid(.data$condition_3 ~ .data$condition_2) +
     theme_bw() +
     labs(y = "Mean Response Time (ms)",
          x = "Probability") +
     scale_x_continuous(breaks = probs,
                        limits = c(0, 1)) +
     coord_flip() +
     scale_colour_discrete(name = conditions[1]) +
     theme(text = element_text(size = 18)) +
     theme(legend.position = "bottom")

   # remove the conditions tag
   id_averaged_data <- id_averaged_data |> select(-.data$condition_1,
                                                  -.data$condition_2,
                                                  -.data$condition_3)
 }

 # if conditions greater than three, plot will not be returned
 if(length(conditions) > 3){
   message("Number of conditions is greater than three. Plot will not
            be returned.")
 }

 return_data <- list(
   id_data = id_data,
   id_averaged_data = id_averaged_data,
   plot = plot
 )
 plot(plot)
 return(return_data)

}
