#' Perform Multiple Query Enrichment on the Rank Response Data and Return Links
#'
#' This function performs multiple query enrichment using gprofiler and returns
#' the short links for the enrichment results. The multi_query is performed
#' separately for each expression source in the data
#'
#' @param rank_response_df A dataframe with rank and target_locus_tag data.
#' @param rank_threshold A numeric value to filter rank.
#'
#' @return A named list where the names are source expressions and the values
#' are short links to gprofiler enrichment results.
#'
#' @importFrom dplyr filter summarise arrange ungroup select
#' @importFrom tibble deframe
#' @importFrom purrr map
#' @importFrom gprofiler2 gost
#'
#' @examples
#' df <- tibble::tibble(
#'   rank = rep(seq(5, 20, 5), each = 5),
#'   target_locus_tag =
#'     rep(list(sample(c(
#'       "YAL001C", "YAL002W", "YAL003W",
#'       "YAL005C", "YAL007C", "YAL008W",
#'       "YAL009W", "YAL010C", "YAL011W"
#'     ), 5)), 20),
#'   experiment = rep(c("expr1", "expr2"), 10),
#'   source_expr = rep(c("experiment1", "experiment2"), each = 10)
#' )
#'
#' @export
multi_query_enrichment_links <- function(rank_response_df, rank_threshold) {

  link_list <- rank_response_df %>%
    ungroup() %>%
    group_by(experiment, source_expr) %>%
    dplyr::filter(rank <= rank_threshold) %>%
    dplyr::summarise(value = list(target_locus_tag)) %>%
    dplyr::arrange(source_expr, experiment) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(source_expr) %>%
    dplyr::group_map(function(data, keys) {
      link <- data %>%
        dplyr::ungroup() %>%
        dplyr::select(experiment, value) %>%
        tibble::deframe() %>%
        map(unlist) %>%
        gprofiler2::gost(
          organism = "scerevisiae",
          multi_query = TRUE,
          highlight = TRUE,
          as_short_link = TRUE
        )

      list(name = keys$source_expr, link = link)
    })

  output <- purrr::map(link_list, ~ .$link)
  names(output) <- unlist(purrr::map(link_list, ~ .$name))

  output
}
