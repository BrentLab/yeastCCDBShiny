#' Create partitions for a vector
#'
#' Given a vector length and the size of each partition, create a vector which
#' represents those partitions. There may be one more partition of size
#' less than the equally divided parts. For example, if the vector length is
#' 14 and the desired partitions are size 3, then there will be 4 partitions
#' of length 3 and one of length 2: 1 1 1 2 2 2 3 3 3 4 4 4 5 5.
#'
#' @param vector_length The total length of the partition vector
#' @param equal_parts The size of each partition
#'
#' @return A vector of `vector_length` divided into `equal_parts` with possibly
#'   one additional vector of size less than `equal_parts`
#'
#' @examples
#' create_partitions(14, 3)
#' create_partitions(10, 4)
#'
#' @export
create_partitions <- function(vector_length, equal_parts = 100) {
  c(
    rep(seq(1, (vector_length / equal_parts)),
      each = equal_parts
    ),
    rep(floor(vector_length / equal_parts) + 1, vector_length %% equal_parts)
  )
}

#' Process data and return a rank_response_df
#'
#' This function processes transcription factor (TF) data and returns a
#' rank_response_df, a data frame containing processed information about TFs
#' and their target genes.
#'
#' @importFrom dplyr rename group_by filter distinct select mutate inner_join
#'   coalesce
#' @importFrom purrr map_dfr
#' @importFrom tidyr replace_na
#' @importFrom stats complete.cases
#'
#' @param expr_df A data frame containing gene expression information
#' @param binding_df A data frame containing binding information
#' @param chipexo_df A data frame containing the chipexo information
#' @param cc_df A data frame containing chromatin capture information
#'
#' @return A rank_response_df data frame containing processed information
#' about TFs and their target genes
#'
#' @examples
#' \dontrun{
#' # Assuming promoter_df, expr_df, binding_df, and cc_df are properly
#' # formatted data frames
#' rank_response_df <- process_tf_data(promoter_df, expr_df, binding_df, cc_df)
#' }
#'
#' @export
process_tf_data <- function(expr_df, binding_df, chipexo_df, cc_df) {
  # set logic controls to assume that expr and binding data exists. If one or
  # the other does not, this is updated in the tryCatch phrases below
  no_expr_data <- FALSE
  no_harbison_data <- FALSE
  no_chipexo_data <- FALSE
  # this gets updated in the binding data tryCatch clause below
  binding_df_fltr <- data.frame()
  chipexo_df_fltr <- data.frame()
  # expression data -----------------------------------------------------------
  if (nrow(expr_df) == 0) {
    no_expr_data <- TRUE
  } else {
    expr_df_fltr <- expr_df %>%
      dplyr::rename(tf_id = tf_id_alias) %>%
      dplyr::group_by(source_expr, tf_id, target_gene_id) %>%
      # for genes with multiple probes, keep only the probe with the lowest
      # p-value
      dplyr::filter(p_expr == min(p_expr)) %>%
      # where there are ties, select the first (arbitrary -- doesn't matter. they
      # are the same)
      dplyr::distinct(tf_id, target_gene_id, .keep_all = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::select(
        tf_id,
        target_gene_id,
        target_locus_tag,
        source_expr,
        effect_expr,
        p_expr
      )
  }


  # outside binding data ------------------------------------------------------
  tryCatch(
    {
      binding_df_fltr <- binding_df %>%
        dplyr::group_by(experiment, tf_id, target_gene_id) %>%
        # for genes with multiple probes, keep only the probe with the lowest
        # p-value
        dplyr::filter(binding_signal == min(binding_signal)) %>%
        # where there are ties, select the first (arbitrary --
        # doesn't matter. they are the same)
        distinct(tf_id, target_gene_id, .keep_all = TRUE) %>%
        ungroup() %>%
        dplyr::mutate(
          experiment_batch = "harbison",
          experiment_replicate = 1,
          enrichment = 1,
          background_total_hops = 1,
          experiment_total_hops = 1,
          background_hops = 1,
          experiment_hops = 1,
          background_source = 1,
          promoter_source = ""
        ) %>%
        dplyr::select(
          tf_id,
          experiment,
          experiment_batch,
          experiment_replicate,
          target_gene_id,
          background_total_hops,
          experiment_total_hops,
          background_hops,
          experiment_hops,
          background_source,
          promoter_source,
          enrichment,
          binding_signal
        )
    },
    warning = function(w) {
      message("Warning: ", conditionMessage(w))
    },
    error = function(e) {
      message("No harbison data")
      binding_df_fltr <- data.frame(
        tf_id = integer(),
        experiment = character(),
        experiment_batch = character(),
        experiment_replicate = integer(),
        target_gene_id = integer(),
        background_total_hops = integer(),
        experiment_total_hops = integer(),
        background_hops = integer(),
        experiment_hops = integer(),
        background_source = character(),
        promoter_source = character(),
        enrichment = numeric(),
        binding_signal = numeric(),
        stringsAsFactors = FALSE
      )
      no_harbison_data <<- TRUE
    }
  )

  # ChipExo data --------------------------------------------------------------
  tryCatch(
    {
      chipexo_df_fltr <- chipexo_df %>%
        # note -strength so that this sorts the same way as the other binding data
        dplyr::mutate(binding_signal = -binding_signal) %>%
        # where there are ties, select the first (arbitrary --
        # doesn't matter. they are the same)
        distinct(tf_id, target_gene_id, .keep_all = TRUE) %>%
        dplyr::mutate(
          experiment_batch = "chipexo",
          experiment_replicate = 1,
          enrichment = 1,
          background_total_hops = 1,
          experiment_total_hops = 1,
          background_hops = 1,
          experiment_hops = 1,
          background_source = 1,
          promoter_source = ""
        ) %>%
        dplyr::select(
          tf_id,
          experiment,
          experiment_batch,
          experiment_replicate,
          target_gene_id,
          background_total_hops,
          experiment_total_hops,
          background_hops,
          experiment_hops,
          background_source,
          promoter_source,
          enrichment,
          binding_signal
        )
    },
    warning = function(w) {
      message("Warning: ", conditionMessage(w))
    },
    error = function(e) {
      message("No chipexo data")
      chipexo_df_fltr <- data.frame(
        tf_id = integer(),
        experiment = character(),
        experiment_batch = character(),
        experiment_replicate = integer(),
        target_gene_id = integer(),
        background_total_hops = integer(),
        experiment_total_hops = integer(),
        background_hops = integer(),
        experiment_hops = integer(),
        background_source = character(),
        promoter_source = character(),
        enrichment = numeric(),
        binding_signal = numeric(),
        stringsAsFactors = FALSE
      )
      no_chipexo_data <<- TRUE
    }
  )


  # calling cards data --------------------------------------------------------
  cc_df_fltr <- cc_df %>%
    dplyr::rename(
      binding_signal = poisson_pval,
      enrichment = callingcards_enrichment,
      experiment = experiment_id
    ) %>%
    dplyr::mutate(experiment = as.character(experiment)) %>%
    dplyr::select(
      tf_id,
      experiment,
      experiment_batch,
      experiment_replicate,
      target_gene_id,
      background_total_hops,
      experiment_total_hops,
      background_hops,
      experiment_hops,
      background_source,
      promoter_source,
      enrichment,
      binding_signal
    )


  # join outside expr and binding data ----------------------------------------
  if (no_expr_data) {
    stop("There is no expression data -- cannot conduct rank response")
  }

  # compile binding data ------------------------------------------------------
  binding_combined <- rbind(cc_df_fltr, binding_df_fltr, chipexo_df_fltr)

  # join expression data
  binding_combined %>%
    dplyr::inner_join(expr_df_fltr,
      by = c("tf_id", "target_gene_id"),
      relationship = "many-to-many"
    )
}

#' Calculate stable rank response for a dataframe
#'
#' Given a dataframe with binding signals and responsiveness, calculate the
#' response ratio for each group of records partitioned by their rank.
#'
#' @importFrom dplyr arrange mutate group_by summarise
#'
#' @param df A dataframe containing binding signals and responsiveness
#' @param binding_expr_source_string A string representing the source of
#'   binding expression
#' @param bin_size The number of records per group (partition)
#' @param separator A string separator used in the binding expression
#'   source string
#'
#' @return A dataframe with response ratios calculated for each group
#'
#' @importFrom dplyr arrange desc mutate group_by summarise
#' @importFrom futile.logger flog.warn
#'
#' @examples
#' # Given a small example dataframe similar to your input
#' df <- data.frame(
#'   binding_signal = c(1, 2, 3, 4, 5),
#'   responsive = c(TRUE, FALSE, TRUE, TRUE, FALSE),
#'   experiment = "test_experiment",
#'   source_expr = "test_source_expr"
#' )
#' stable_rank_response(df, "test_experiment;test_source_expr", 2, ";")
#'
#' @export
stable_rank_response <- function(df,
                                 experiment,
                                 expr_src) {
  df %>%
    ungroup() %>%
    dplyr::group_by(rank) %>%
    dplyr::summarise(
      n_responsive_in_rank = sum(responsive),
      random = unique(random)
    ) %>%
    dplyr::mutate(n_successes = cumsum(n_responsive_in_rank)) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      test_result = list(binom.test(n_successes,
        rank,
        p = random
      )),
      response_ratio = test_result$estimate,
      p_value = test_result$p.value,
      ci_lower = test_result$conf.int[1],
      ci_upper = test_result$conf.int[2]
    ) %>%
    dplyr::select(-test_result) %>%
    dplyr::mutate(binding_src = experiment, source_expr = expr_src) %>%
    ungroup()
}


#' Calculate rank response ratio summary
#'
#' Given a dataframe with binding signals, effect expression, and p-values,
#' calculate the rank response ratio for each group and summarize the results.
#'
#' @import dplyr
#' @importFrom tidyr pivot_wider unite
#' @importFrom purrr map2
#'
#' @param df A dataframe with binding signals, effect expression, and p-values
#' @param effect_expr_thres A threshold for effect expression (default: 0)
#' @param p_expr_thres A threshold for p-values (default: 0.05)
#' @param normalize A boolean indicating whether to normalize the results (default: FALSE)
#' @param bin_size The number of records per group (partition)
#' @param separator A string separator used in the binding expression source string
#'
#' @return A list containing two dataframes: rr with response ratios calculated
#'   for each group, and random with the random expectation values

#' @examples
#' # Given a small example dataframe similar to your input
#' df <- data.frame(
#'   experiment = rep(c("exp1", "exp2"), each = 6),
#'   source_expr = rep(c("src1", "src2"), times = 6),
#'   binding_signal = runif(6, 0, .07),
#'   effect_expr = c(
#'     0.1, 0.2, -0.3, -0.4, 0.5, 0.6, 0.1,
#'     0.2, -0.3, -0.4, 0.5, 0.6
#'   ),
#'   p_expr = c(
#'     0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.01,
#'     0.02, 0.03, 0.04, 0.05, 0.06
#'   )
#' )
#' rank_response_ratio_summarize(df, bin_size = 3, separator = ";")
#'
#' @export
rank_response_ratio_summarize <- function(df,
                                          effect_expr_thres = 0,
                                          p_expr_thres = 0.05,
                                          normalize = FALSE,
                                          bin_size = 5) {
  grouped_df <- df %>% dplyr::group_by(experiment, source_expr)

  min_responsive <-
    if (normalize == TRUE) {
      grouped_df %>%
        dplyr::filter(abs(effect_expr) >
          effect_expr_thres, p_expr < p_expr_thres) %>%
        dplyr::tally() %>%
        dplyr::pull(n) %>%
        min()
    } else {
      Inf
    }

  # add a field 'responsive' to the dataframe. A gene is called responsive if
  # it passes the threshold filters, and the group rank is less than the
  # min_responsive (Inf if normalized is false, meaning all genes passing
  # thresholds are marked responsive)
  grouped_df <- grouped_df %>%
    dplyr::arrange(dplyr::desc(abs(effect_expr)), .by_group = TRUE) %>%
    dplyr::mutate(
      responsive =
        ifelse(
          abs(effect_expr) > effect_expr_thres &
            p_expr < p_expr_thres &
            dplyr::row_number() <= min_responsive,
          TRUE,
          FALSE
        )
    )

  random_expectation_df_full <- grouped_df %>%
    dplyr::group_by(experiment, source_expr, responsive) %>%
    dplyr::tally() %>%
    tidyr::pivot_wider(
      names_from = responsive,
      values_from = n
    ) %>%
    # if there are on TRUE/FALSE values for a given combination of
    # expr and binding, replace that with 0
    replace(is.na(.), 0) %>%
    dplyr::rename(unresponsive = `FALSE`, responsive = `TRUE`) %>%
    dplyr::mutate(random = responsive / (unresponsive + responsive))

  rank_response_df <- grouped_df %>%
    droplevels() %>%
    dplyr::left_join(
      dplyr::select(
        random_expectation_df_full,
        experiment, source_expr, random
      )
    ) %>%
    dplyr::arrange(
      experiment, source_expr,
      dplyr::desc(enrichment), binding_signal
    ) %>%
    dplyr::arrange(dplyr::desc(enrichment), binding_signal) %>%
    # note the min() between the group length and bin_size -- this is to
    # adjust the bin_size to the number of genes in the event of a short
    # group
    dplyr::mutate(
      rank = create_partitions(n(), min(n(), bin_size)) * min(n(), bin_size)
    )

  rr_df <- rank_response_df %>%
    group_map(function(data, keys) {
      stable_rank_response(data, keys$experiment, keys$source_expr)
    }) %>%
    bind_rows()

  random_expectation_df_summary <- random_expectation_df_full %>%
    dplyr::mutate(
      experiment =
        ifelse(!experiment %in% c("chipexo_yiming", "harbison"),
          "calling_cards", experiment
        )
    ) %>%
    distinct() %>%
    dplyr::rename(binding_src = experiment)

  list(
    rr = rr_df,
    random = random_expectation_df_summary,
    full_rr = rank_response_df
  )
}

#' Create rank response plot
#'
#' This function creates a rank response plot using the ggplot2 package. It
#' takes a rank response summary as input and returns a ggplot object.
#'
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom dplyr select ungroup group_by group_map filter pull
#' @importFrom purrr map
#'
#' @param rank_response_summary A rank response summary object
#' @param confidence_intervals Boolean. Default to FALSE. Set to TRUE to plot
#'   points with confidence interval ranges rather than lines. Note that
#'   the line plots have the conf.int info in the hovertip
#'
#' @return A ggplot object representing the rank response plot
#'
#' @examples
#' \dontrun{
#' # Assuming rank_response_summary is a properly formatted object
#' plot <- plot_rank_response(rank_response_summary)
#' print(plot)
#' }
#'
#' @export
plot_rank_response <- function(rank_response_summary,
                               confidence_intervals = FALSE) {
  plot_list <- rank_response_summary$rr %>%
    dplyr::ungroup() %>%
    dplyr::group_by(source_expr) %>%
    dplyr::group_map(function(data, keys) {
      random_info <- rank_response_summary$random %>%
        filter(
          binding_src == "calling_cards",
          source_expr == keys$source_expr
        ) %>%
        mutate(total = unresponsive + responsive) %>%
        select(responsive, total)

      plot_title <- paste(
        "Expression Source:", keys$source_expr,
        "\nResponsive : Total (CC gene set): ",
        random_info$responsive, " : ",
        random_info$total
      )

      p <- plotly::plot_ly(
        data = data,
        x = ~rank,
        y = ~response_ratio,
        color = ~binding_src,
        text = ~ format(p_value, digits = 3),
        hoverinfo = "text",
        hovertext = ~ paste(
          "experiment: ", binding_src,
          "\nrank_bin: ", rank,
          "\nresponse_ratio: ", response_ratio,
          "\nconf.int: ", ci_lower, " - ", ci_upper,
          "\np-value:", p_value
        )
      )

      if (confidence_intervals) {
        p <- p %>% plotly::add_trace(
          type = "scatter",
          mode = "markers",
          error_y = list(
            type = "data",
            symmetric = FALSE,
            array = ~ ci_upper - response_ratio,
            arrayminus = ~ response_ratio - ci_lower
          )
        )
      } else {
        p <- p %>% plotly::add_trace(
          type = "scatter",
          mode = "lines"
        )
      }
      p <- p %>%
        plotly::add_trace(
          y = ~random,
          type = "scatter",
          mode = "lines",
          color = I("black"),
          linetype = I("dot"),
          name = "random",
          hoverinfo = "y"
        ) %>%
        plotly::layout(
          title = plot_title,
          xaxis = list(
            title = "Rank",
            range = c(0, 150),
            tickmode = "array",
            tickvals = seq(0, 150, 5)
          ),
          yaxis = list(
            title = "Response Ratio",
            range = c(0, 1),
            tickmode = "array",
            tickvals = seq(0, 1, .1)
          ),
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            y = -0.2
          ),
          showlegend = TRUE
        )

      list(name = keys$source_expr, plot = p)
    })

  output <- purrr::map(plot_list, ~ .$plot)
  names(output) <- unlist(purrr::map(plot_list, ~ .$name))

  output$random_df <- rank_response_summary$random

  output
}
# plot_rank_response <- function(rank_response_summary,
#                                color_vector,
#                                plot_title = "") {
#   stopifnot(
#     length(unique(rank_response_summary$rr$binding_src)) ==
#       length(color_vector)
#   )
#
#   # use scale_color_manual() with the color_vector to set the colors of the binding_src levels
#   harbison_plt_obj = rank_response_summary$rr %>%
#     filter(binding_src != 'chipexo_yiming') %>%
#   ggplot2::ggplot(
#     ggplot2::aes(rank, response_ratio,
#       color = binding_src))
#
#   chipexo_plt_obj = rank_response_summary$rr  %>%
#     filter(binding_src != 'harbison') %>%
#     ggplot2::ggplot(
#       ggplot2::aes(rank, response_ratio,
#                    color = binding_src))
#
#     add_layers = function(plt_obj,
#                           remove_binding_src){
#
#       random_df = rank_response_summary$random %>%
#         filter(binding_src != remove_binding_src)
#
#       plt_obj +
#     ggplot2::geom_line() +
#     ggplot2::geom_hline(
#       data = random_df,
#       ggplot2::aes(yintercept = random, linetype = binding_src)) +
#     ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, .1)) +
#     ggplot2::scale_x_continuous(breaks = seq(0, 150, 10)) +
#     ggplot2::coord_cartesian(xlim = c(0, 150)) +
#     ggplot2::theme(text = ggplot2::element_text(size = 14)) +
#     ggplot2::labs(color = "Binding Data Source") +
#     ggplot2::scale_color_manual(values = color_vector) +
#     ggplot2::ggtitle(plot_title) +
#     ggplot2::facet_wrap(~source_expr)
#     }
#
#     list(
#       harbison = add_layers(harbison_plt_obj, 'chipexo_yiming'),
#       chipexo = add_layers(chipexo_plt_obj, 'harbison'),
#       random_df = rank_response_summary$random
#     )
# }
