#' Plot a Claim Space
#'
#' Plot selected observed, implied, and equalised structures from a
#' claim_space object.
#'
#' The impact matrix M is always shown. Salience structures are placed
#' across the top row, while evaluation structures are stacked beneath M.
#'
#' @param x A claim_space object.
#' @param s Character vector containing any of "S", "S^", and "S*".
#' @param e Character vector containing any of "E", "E^", and "E*".
#' @param measure One of "positive", "negative", "net", or "gross".
#' @param labels Optional named character vector used to replace displayed
#'   structure labels. Names must be one of "S", "S^", "S*", "M", "E",
#'   "E^", or "E*".
#' @param show_values Logical. Whether to display values in cells.
#' @param digits Number of decimal places for displayed values.
#' @param low Colour for low values.
#' @param mid Colour for the midpoint.
#' @param high Colour for high values.
#' @param midpoint Value corresponding to the midpoint colour.
#' @param limits Optional limits for the fill scale.
#' @param na.value Colour used for missing values.
#'
#' @return A ggplot object.
#'
#' @export
plot_claims <- function(
  x,
  s = c("S", "S^", "S*"),
  e = c("E", "E^", "E*"),
  measure = "net",
  labels = NULL,
  show_values = TRUE,
  digits = 1,
  low = "#D08770",
  mid = "#EBCB8B",
  high = "#A3BE8C",
  midpoint = 0,
  limits = NULL,
  na.value = "grey80"
) {
  # ------------------------------------------------------------
  # Validate object
  # ------------------------------------------------------------

  if (!inherits(x, "claim_space")) {
    stop(
      "x must be a claim_space object.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # Validate requested structures
  # ------------------------------------------------------------

  valid_s <- c(
    "S",
    "S^",
    "S*"
  )

  valid_e <- c(
    "E",
    "E^",
    "E*"
  )

  valid_measures <- c(
    "positive",
    "negative",
    "net",
    "gross"
  )

  if (
    length(s) == 0 ||
      any(!s %in% valid_s) ||
      anyDuplicated(s)
  ) {
    stop(
      "s must contain unique values from S, S^, and S*.",
      call. = FALSE
    )
  }

  if (
    length(e) == 0 ||
      any(!e %in% valid_e) ||
      anyDuplicated(e)
  ) {
    stop(
      "e must contain unique values from E, E^, and E*.",
      call. = FALSE
    )
  }

  if (
    length(measure) != 1L ||
      !measure %in% valid_measures
  ) {
    stop(
      "measure must be one of positive, negative, net, or gross.",
      call. = FALSE
    )
  }

  # ------------------------------------------------------------
  # Structure labels
  #
  # Internal names remain mathematical:
  # S, S^, S*, M, E, E^, E*
  #
  # These labels only control what is displayed on the plot.
  # ------------------------------------------------------------

  default_labels <- c(
    S = "S",
    `S^` = "S^",
    `S*` = "S*",
    M = "M",
    E = "E",
    `E^` = "E^",
    `E*` = "E*"
  )

  if (!is.null(labels)) {
    if (
      is.null(names(labels)) ||
        any(names(labels) == "")
    ) {
      stop(
        "labels must be a named character vector.",
        call. = FALSE
      )
    }

    if (!is.character(labels)) {
      stop(
        "labels must be a named character vector.",
        call. = FALSE
      )
    }

    unknown <- setdiff(
      names(labels),
      names(default_labels)
    )

    if (length(unknown) > 0) {
      stop(
        "Unknown label(s): ",
        paste(unknown, collapse = ", "),
        ".",
        call. = FALSE
      )
    }

    default_labels[names(labels)] <- labels
  }

  structure_labels <- default_labels

  # ------------------------------------------------------------
  # Helper
  # ------------------------------------------------------------

  get_structure <- function(group) {
    claim_group(
      x,
      group = group,
      measure = measure
    )
  }

  # ------------------------------------------------------------
  # S panels
  #
  # S is a vector over issues.
  # ------------------------------------------------------------

  df_S <- lapply(
    s,
    function(group) {
      value <- get_structure(group)

      data.frame(
        Issue = names(value),
        X_Var = "",
        RowGroup = "Top",
        ColGroup = group,
        Value = as.numeric(value),
        stringsAsFactors = FALSE
      )
    }
  )

  df_S <- do.call(
    rbind,
    df_S
  )

  # ------------------------------------------------------------
  # M panel
  #
  # M is issues x policies.
  #
  # This is now the canonical orientation of the CSA object,
  # so no transpose is required.
  # ------------------------------------------------------------

  M <- get_structure("M")

  df_M <- expand.grid(
    Issue = rownames(M),
    X_Var = colnames(M),
    stringsAsFactors = FALSE
  )

  df_M$Value <- as.vector(M)
  df_M$RowGroup <- "Top"
  df_M$ColGroup <- "M"

  df_M <- df_M[
    ,
    c(
      "Issue",
      "X_Var",
      "RowGroup",
      "ColGroup",
      "Value"
    )
  ]

  # ------------------------------------------------------------
  # E panels
  #
  # E is a vector over policies.
  #
  # Each E panel sits underneath M and therefore shares the
  # policy X axis with M.
  # ------------------------------------------------------------

  e_row_groups <- paste0(
    "E",
    seq_along(e)
  )

  df_E <- lapply(
    seq_along(e),
    function(i) {
      group <- e[[i]]

      value <- get_structure(
        group
      )

      data.frame(
        Issue = "",
        X_Var = names(value),
        RowGroup = e_row_groups[[i]],
        ColGroup = "M",
        Value = as.numeric(value),
        stringsAsFactors = FALSE
      )
    }
  )

  df_E <- do.call(
    rbind,
    df_E
  )

  # ------------------------------------------------------------
  # Combine
  # ------------------------------------------------------------

  df_all <- rbind(
    df_S,
    df_M,
    df_E
  )

  # ------------------------------------------------------------
  # Shared axis levels
  # ------------------------------------------------------------

  issue_levels <- unique(
    c(
      "",
      rev(rownames(M))
    )
  )

  x_levels <- unique(
    c(
      "",
      colnames(M)
    )
  )

  df_all$Issue <- factor(
    df_all$Issue,
    levels = issue_levels
  )

  df_all$X_Var <- factor(
    df_all$X_Var,
    levels = x_levels
  )

  df_all$RowGroup <- factor(
    df_all$RowGroup,
    levels = c(
      "Top",
      e_row_groups
    )
  )

  df_all$ColGroup <- factor(
    df_all$ColGroup,
    levels = c(
      s,
      "M"
    )
  )

  # ------------------------------------------------------------
  # Facet labels
  # ------------------------------------------------------------

  e_labels <- stats::setNames(
    structure_labels[e],
    e_row_groups
  )

  col_labels <- stats::setNames(
    structure_labels[c(s, "M")],
    c(s, "M")
  )

  # ------------------------------------------------------------
  # Plot
  # ------------------------------------------------------------

  p <- ggplot2::ggplot(
    df_all,
    ggplot2::aes(
      x = X_Var,
      y = Issue,
      fill = Value
    )
  ) +
    ggplot2::geom_tile(
      colour = "white",
      linewidth = 1
    )

  if (show_values) {
    p <- p +
      ggplot2::geom_text(
        ggplot2::aes(
          label = round(
            Value,
            digits
          )
        ),
        size = 6,
        colour = "black",
        fontface = "bold"
      )
  }

  p <- p +
    ggplot2::scale_fill_gradient2(
      low = low,
      mid = mid,
      high = high,
      midpoint = midpoint,
      limits = limits,
      na.value = na.value
    ) +
    ggplot2::facet_grid(
      RowGroup ~ ColGroup,
      scales = "free",
      space = "free",
      labeller = ggplot2::labeller(
        RowGroup = c(
          Top = "",
          e_labels
        ),
        ColGroup = col_labels
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(
        size = 8,
        face = "bold"
      ),
      axis.text.y = ggplot2::element_text(
        size = 8,
        face = "bold"
      ),
      axis.title = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      strip.placement = "outside",
      strip.text = ggplot2::element_text(
        size = 12,
        face = "bold",
        margin = ggplot2::margin(
          5,
          5,
          5,
          5
        )
      ),
      strip.background = ggplot2::element_blank(),
      panel.spacing = grid::unit(
        0.3,
        "lines"
      ),
      legend.position = "none"
    )

  p
}
