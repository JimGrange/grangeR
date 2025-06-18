#' Compute Conditional Accuracy Functions (CAF) from Response Time Data
#'
#' This function computes CAF values from response time and accuracy data,
#' optionally grouping by conditions.
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
#' @param n_bins An integer stating how many bins to include in the CAF.
#'
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr inner_join
#' @importFrom dplyr mutate
#' @importFrom dplyr ntile
#' @importFrom dplyr rename
#' @importFrom dplyr rename_with
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom ggplot2 ggplot aes coord_flip element_text geom_errorbar
#' geom_errorbarh geom_point geom_line theme theme_bw labs scale_colour_discrete
#' scale_x_continuous facet_grid
#' @importFrom rlang .data
#' @importFrom stats sd quantile
#' @importFrom tidyr unnest_wider
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of
#' @importFrom stringr str_remove
#'
#' @export

get_caf <- function(data,
                    rt_var = "rt",
                    accuracy_var = "accuracy",
                    id_var = NULL,
                    conditions = NULL,
                    n_bins = 4){

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


  # get the CAF values for each individual
  id_data <- data |>
    group_by(.data$id,
             across(all_of(conditions))) |>
    mutate(rt_bin = ntile(.data$rt, n_bins)) |>
    group_by(.data$id,
             across(all_of(conditions)),
             rt_bin) |>
    summarise(
      m_rt = mean(.data$rt),
      m_accuracy = mean(.data$accuracy),
      .groups = "drop"
    )

  # get the CAF data averaged across subjects
  if(is.null(id_var)){
    id_averaged_data <- id_data |>
      rename(mean_rt = .data$m_rt,
             mean_accuracy = .data$m_accuracy) |>
      mutate(se_rt = 0,
             se_acc = 0)
  } else {
    id_averaged_data <- id_data |>
      group_by(across(all_of(conditions)),
               .data$rt_bin) |>
      summarise(mean_rt = mean(.data$m_rt),
                se_rt = sd(.data$m_rt) / sqrt(length(.data$m_rt)),
                mean_acc = mean(.data$m_accuracy),
                se_acc = sd(.data$m_accuracy) /
                  sqrt(length(.data$m_accuracy)),
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
      ggplot(aes(x = .data$mean_rt,
                 y = .data$mean_acc)) +
      geom_point(size = 3) +
      geom_line(linewidth = 1) +
      geom_errorbar(aes(ymin = .data$mean_acc - .data$se_acc,
                        ymax = .data$mean_acc + .data$se_acc),
                    width = 0.0) +
      geom_errorbarh(aes(xmin = .data$mean_rt - .data$se_rt,
                         xmax = .data$mean_rt + .data$se_rt),
                     height = 0.0) +
      theme_bw() +
      labs(x = "Mean Response Time (ms)",
           y = "Proportion Accuracy") +
      theme(text = element_text(size = 18)) +
      theme(legend.position = "bottom")
  }

  # plot if there is one condition
  if(length(conditions) == 1){
    id_averaged_data$condition <- as.factor(id_averaged_data[[conditions]])

    plot <- id_averaged_data |>
      ggplot(aes(x = .data$mean_rt,
                 y = .data$mean_acc,
                 group = .data$condition)) +
      geom_point(size = 3,
                 aes(colour = .data$condition)) +
      geom_line(aes(colour = .data$condition),
                linewidth = 1) +
      geom_errorbar(aes(colour = .data$condition,
                        ymin = .data$mean_acc - .data$se_acc,
                        ymax = .data$mean_acc + .data$se_acc),
                    width = 0.0) +
      geom_errorbarh(aes(colour = .data$condition,
                         xmin = .data$mean_rt - .data$se_rt,
                         xmax = .data$mean_rt + .data$se_rt),
                     height = 0.0) +
      theme_bw() +
      scale_colour_discrete(name = conditions) +
      labs(x = "Mean Response Time (ms)",
           y = "Proportion Accuracy") +
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
      ggplot(aes(x = .data$mean_rt,
                 y = .data$mean_acc,
                 group = .data$condition_1)) +
      geom_point(size = 3,
                 aes(colour = .data$condition_1)) +
      geom_line(aes(colour = .data$condition_1),
                linewidth = 1) +
      geom_errorbar(aes(colour = .data$condition_1,
                        ymin = .data$mean_acc - .data$se_acc,
                        ymax = .data$mean_acc + .data$se_acc),
                    width = 0.0) +
      geom_errorbarh(aes(colour = .data$condition_1,
                         xmin = .data$mean_rt - .data$se_rt,
                         xmax = .data$mean_rt + .data$se_rt),
                     height = 0.0) +
      theme_bw() +
      facet_grid(.~.data$condition_2) +
      scale_colour_discrete(name = conditions) +
      labs(x = "Mean Response Time (ms)",
           y = "Proportion Accuracy") +
      theme(text = element_text(size = 18)) +
      theme(legend.position = "bottom")
  }



  # plot if there are three conditions
  if(length(conditions) == 3){

    id_averaged_data$condition_1 <- as.factor(id_averaged_data[[conditions[1]]])
    id_averaged_data$condition_2 <- as.factor(id_averaged_data[[conditions[2]]])
    id_averaged_data$condition_3 <- as.factor(id_averaged_data[[conditions[3]]])

    plot <- id_averaged_data |>
      ggplot(aes(x = .data$mean_rt,
                 y = .data$mean_acc,
                 group = .data$condition_1)) +
      geom_point(size = 3,
                 aes(colour = .data$condition_1)) +
      geom_line(aes(colour = .data$condition_1),
                linewidth = 1) +
      geom_errorbar(aes(colour = .data$condition_1,
                        ymin = .data$mean_acc - .data$se_acc,
                        ymax = .data$mean_acc + .data$se_acc),
                    width = 0.0) +
      geom_errorbarh(aes(colour = .data$condition_1,
                         xmin = .data$mean_rt - .data$se_rt,
                         xmax = .data$mean_rt + .data$se_rt),
                     height = 0.0) +
      theme_bw() +
      facet_grid(.data$condition_3 ~ .data$condition_2) +
      scale_colour_discrete(name = conditions) +
      labs(x = "Mean Response Time (ms)",
           y = "Proportion Accuracy") +
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
