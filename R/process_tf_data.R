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
#' @param promoter_df A data frame containing promoter information
#' @param expr_df A data frame containing gene expression information
#' @param binding_df A data frame containing binding information
#' @param cc_df A data frame containing chromatin capture information
#'
#' @return A rank_response_df data frame containing processed information
#' about TFs and their target genes
#'
#' @examples
#' \dontrun{# Assuming promoter_df, expr_df, binding_df, and cc_df are properly
#' # formatted data frames
#' rank_response_df <- process_tf_data(promoter_df, expr_df, binding_df, cc_df)}
#'
#' @export
process_tf_data = function(promoter_df, expr_df,
                           binding_df,cc_df){
  expr_df_fltr = expr_df %>%
    dplyr::rename(tf_id=tf_id_alias) %>%
    dplyr::group_by(source_expr, tf_id, target_gene_id) %>%
    # for genes with multiple probes, keep only the probe with the lowest
    # p-value
    dplyr::filter(p_expr == min(p_expr)) %>%
    # where there are ties, select the first (arbitrary -- doesn't matter. they
    # are the same)
    dplyr::distinct(tf_id,target_gene_id, .keep_all = TRUE)

  binding_df_fltr = binding_df %>%
    dplyr::group_by(experiment, tf_id, target_gene_id) %>%
    # for genes with multiple probes, keep only the probe with the lowest
    # p-value
    dplyr::filter(binding_signal == min(binding_signal)) %>%
    # where there are ties, select the first (arbitrary -- doesn't matter. they
    # are the same)
    distinct(tf_id,target_gene_id, .keep_all = TRUE)

  cc_df_fltr = cc_df %>%
    dplyr::rename(tf_id=tf_id_alias) %>%
    # for the data which was uploaded from yiming's original processing, the same
    # promoter is listed twice in the ccf. As a result, when the data was added
    # and then joined with the associated_feature, a given gene may have 2
    # entries, meaning the promoter gets 4 entries, as opposed to 1 entry per
    # gene and up to 2 genes per promoter
    dplyr::distinct(experiment,target_gene_id, .keep_all = TRUE)

  experiment_map = cc_df %>%
    dplyr::select(experiment, experiment_batch, experiment_batch_replicate) %>%
    dplyr::distinct()

  experiments = unique(cc_df$experiment)

  process_cc_experiment = function(experiment) {

    cc_df_subset = cc_df_fltr %>%
      dplyr::filter(experiment == !!experiment) %>%
      dplyr::distinct(promoter_id, .keep_all = TRUE)

    col_list = list(
      tf_id = unique(cc_df_subset$tf_id),
      tf_gene = unique(cc_df$tf_gene),
      tf_locus_tag = unique(cc_df$tf_locus_tag),
      experiment = experiment,
      background = unique(cc_df$background),
      promoter_source = unique(cc_df$promoter_source),
      bg_hops = 0,
      expr_hops = 0,
      poisson_pval = 1,
      hypergeom_pval = 1
    )

    promoter_df %>%
      dplyr::left_join(cc_df_subset,by = "promoter_id") %>%
      dplyr::mutate(target_gene_id = dplyr::coalesce(target_gene_id.x, target_gene_id.y),
             target_locus_tag = dplyr::coalesce(target_locus_tag.x, target_locus_tag.y),
             target_gene = dplyr::coalesce(target_gene.x, target_gene.y)) %>%
      dplyr::select(-c(target_gene_id.x, target_gene_id.y,
                target_locus_tag.x, target_locus_tag.y,
                target_gene.x, target_gene.y,
                experiment_batch, experiment_batch_replicate)) %>%
      tidyr::replace_na(col_list) %>%
      dplyr::left_join(experiment_map) %>%
      dplyr::left_join(expr_df_fltr, multiple='all') %>%
      dplyr::rename(binding_signal=poisson_pval) %>%
      dplyr::select(tf_id, tf_locus_tag, tf_gene,
             target_gene_id, target_locus_tag, target_gene,
             experiment, binding_signal,
             effect_expr, p_expr, source_expr)
  }

  outside_expr_binding_df = expr_df_fltr %>%
    dplyr::inner_join(binding_df_fltr) %>%
    dplyr::select(tf_id, tf_locus_tag, tf_gene,
           target_gene_id, target_locus_tag, target_gene,
           experiment, binding_signal,
           effect_expr, p_expr, source_expr)


  result_df <- purrr::map_dfr(experiments, process_cc_experiment)

  compiled_result_df = rbind(result_df, outside_expr_binding_df)

  rank_response_df = compiled_result_df %>%
    dplyr::filter(stats::complete.cases(.))

  # return the rank_response_df
  rank_response_df

}
