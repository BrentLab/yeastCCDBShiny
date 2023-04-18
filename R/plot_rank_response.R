#' Create rank response plot
#'
#' This function creates a rank response plot using the ggplot2 package. It
#' takes a rank response summary as input and returns a ggplot object.
#'
#' @import ggplot2
#'
#' @param rank_response_summary A rank response summary object
#' @param color_vector a vector of colors -- must be same length as unique
#'   binding_src levels
#' @param plot_title A title for the plot. Defaults to empty string.
#'
#' @return A ggplot object representing the rank response plot
#'
#' @examples
#' \dontrun{# Assuming rank_response_summary is a properly formatted object
#' plot <- plot_rank_response(rank_response_summary)
#' print(plot)}
#'
#' @export
plot_rank_response = function(rank_response_summary,
                              color_vector,
                              plot_title=""){

  stopifnot(
    length(unique(rank_response_summary$rr$binding_src)) ==
      length(color_vector)
  )

  # use scale_color_manual() with the color_vector to set the colors of the binding_src levels
  ggplot2::ggplot(rank_response_summary$rr,
                  ggplot2::aes(rank, response_ratio,
                               color=binding_src,
                               linetype = binding_src=='harbison')) +
    ggplot2::geom_line() +
    ggplot2::geom_hline(data=rank_response_summary$random,
                        ggplot2::aes(yintercept=random),linetype='dotted') +
    ggplot2::scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.1)) +
    ggplot2::coord_cartesian(xlim = c(0,150)) +
    ggplot2::scale_x_continuous(breaks=seq(0,150,10)) +
    ggplot2::theme(text=ggplot2::element_text(size=11)) +
    ggplot2::labs(color='Binding Data Source',
                  linetype="") +
    ggplot2::scale_color_manual(values = color_vector) +
    ggplot2::ggtitle(plot_title) +
    ggplot2::facet_wrap(~source_expr)
}
