#' Extract a claim-space quantity
#'
#' @param x A `claim_space` object.
#' @param group One of `E`, `M`, `S`, `E*`, `E^`, `S*`, or `S^`.
#' @param measure One of `positive`, `negative`, `net`, or `gross`.
#' @return A numeric vector or matrix.
#' @export
claim_group <- function(
    x,
    group,
    measure = "net"
) {
    if (!inherits(x, "claim_space")) {
        stop(
            "x must be a claim_space object.",
            call. = FALSE
        )
    }

    valid_groups <- c(
        "E",
        "M",
        "S",
        "E*",
        "E^",
        "S*",
        "S^"
    )

    valid_measures <- c(
        "positive",
        "negative",
        "net",
        "gross"
    )

    if (
        length(group) != 1L ||
        !group %in% valid_groups
    ) {
        stop(
            "group must be one of E, M, S, E*, E^, S*, or S^.",
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

    if (group %in% c("E", "M", "S")) {
        return(
            x$measures[[measure]][[group]]
        )
    }

    x$derived[[measure]][[group]]
}


#' Convert a claim space to a data frame
#'
#' Returns all primary and derived quantities in long form. This is useful for
#' inspection and for downstream custom visualisations.
#'
#' @param x A `claim_space` object.
#' @param measure One of `positive`, `negative`, `net`, or `gross`.
#' @param ... Additional arguments.
#' @return A data frame.
#' @export
as.data.frame.claim_space <- function(
    x,
    measure = "net",
    ...
) {
    groups <- c(
        "S",
        "S*",
        "S^",
        "M",
        "E",
        "E*",
        "E^"
    )

    rows <- lapply(
        groups,
        function(group) {

            value <- claim_group(
                x,
                group,
                measure
            )

            if (group %in% c("S", "S*", "S^")) {

                data.frame(
                    group = group,
                    dimension = "S",
                    issue = names(value),
                    policy = NA_character_,
                    value = as.numeric(value),
                    stringsAsFactors = FALSE
                )

            } else if (group %in% c("E", "E*", "E^")) {

                data.frame(
                    group = group,
                    dimension = "E",
                    issue = NA_character_,
                    policy = names(value),
                    value = as.numeric(value),
                    stringsAsFactors = FALSE
                )

            } else {

                # M is always:
                # rows    = issues
                # columns = policies

                grid <- expand.grid(
                    issue = rownames(value),
                    policy = colnames(value),
                    KEEP.OUT.ATTRS = FALSE,
                    stringsAsFactors = FALSE
                )

                grid$group <- group
                grid$dimension <- "M"

                grid$value <- as.vector(
                    value
                )

                grid[
                    ,
                    c(
                        "group",
                        "dimension",
                        "issue",
                        "policy",
                        "value"
                    )
                ]
            }
        }
    )

    do.call(
        rbind,
        rows
    )
}
