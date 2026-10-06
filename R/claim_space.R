#' Construct a claim space
#'
#' Builds a fully computed `claim_space` object from positive and negative
#' issue-signalling, impact, and evaluation claims.
#'
#' All algebraic transformations are calculated when the object is created.
#' This means downstream analysis and plotting functions operate only on
#' precomputed claim-space quantities.
#'
#' @param S_plus Numeric issue-signalling vector for positive claims.
#' @param S_minus Numeric issue-signalling vector for negative claims.
#' @param M_plus Numeric issue-by-policy matrix for positive impact claims.
#' @param M_minus Numeric issue-by-policy matrix for negative impact claims.
#' @param E_plus Numeric policy evaluation vector for positive claims.
#' @param E_minus Numeric policy evaluation vector for negative claims.
#' @param issues Character vector of issue names. Defaults to vector/matrix names
#'   where available.
#' @param policies Character vector of policy names. Defaults to vector/matrix
#'   names where available.
#' @param svd_tol Relative tolerance used for the Moore-Penrose pseudoinverse.
#'
#' @return An object of class `claim_space`.
#' @export
claim_space <- function(
    S_plus,
    S_minus,
    M_plus,
    M_minus,
    E_plus,
    E_minus,
    issues = NULL,
    policies = NULL,
    svd_tol = 1e-10
) {

    # ------------------------------------------------------------
    # Resolve names
    #
    # Explicit issues/policies take precedence over names already
    # attached to the input vectors and matrices.
    # ------------------------------------------------------------

    if (is.null(issues)) {
        issues <- names(S_plus)

        if (is.null(issues) && !is.null(rownames(M_plus))) {
            issues <- rownames(M_plus)
        }
    }

    if (is.null(policies)) {
        policies <- names(E_plus)

        if (is.null(policies) && !is.null(colnames(M_plus))) {
            policies <- colnames(M_plus)
        }
    }

    # ------------------------------------------------------------
    # Validate supplied names
    # ------------------------------------------------------------

    if (is.null(issues)) {
        stop(
            "Issues must be named, or supplied explicitly through `issues`.",
            call. = FALSE
        )
    }

    if (is.null(policies)) {
        stop(
            "Policies must be named, or supplied explicitly through `policies`.",
            call. = FALSE
        )
    }

    if (!is.character(issues)) {
        stop(
            "issues must be a character vector.",
            call. = FALSE
        )
    }

    if (!is.character(policies)) {
        stop(
            "policies must be a character vector.",
            call. = FALSE
        )
    }

    if (length(issues) != length(S_plus)) {
        stop(
            "issues must have the same length as S.",
            call. = FALSE
        )
    }

    if (length(policies) != length(E_plus)) {
        stop(
            "policies must have the same length as E.",
            call. = FALSE
        )
    }

    # ------------------------------------------------------------
    # Apply canonical names before validation
    # ------------------------------------------------------------

    names(S_plus) <- issues
    names(S_minus) <- issues

    names(E_plus) <- policies
    names(E_minus) <- policies

    if (is.matrix(M_plus)) {
        rownames(M_plus) <- issues
        colnames(M_plus) <- policies
    }

    if (is.matrix(M_minus)) {
        rownames(M_minus) <- issues
        colnames(M_minus) <- policies
    }

    # ------------------------------------------------------------
    # Validate vector inputs
    # ------------------------------------------------------------

    validate_vector_pair <- function(
        plus,
        minus,
        name
    ) {
        if (!is.numeric(plus) || !is.numeric(minus)) {
            stop(
                name,
                " must contain numeric vectors.",
                call. = FALSE
            )
        }

        if (length(plus) != length(minus)) {
            stop(
                name,
                " positive and negative vectors must have the same length.",
                call. = FALSE
            )
        }

        if (is.null(names(plus)) || is.null(names(minus))) {
            stop(
                name,
                " vectors must have names.",
                call. = FALSE
            )
        }

        if (!identical(names(plus), names(minus))) {
            stop(
                name,
                " positive and negative vectors must have identical names.",
                call. = FALSE
            )
        }
    }

    validate_vector_pair(
        S_plus,
        S_minus,
        "S"
    )

    validate_vector_pair(
        E_plus,
        E_minus,
        "E"
    )

    # ------------------------------------------------------------
    # Validate M
    #
    # M is always:
    #
    #     rows    = issues
    #     columns = policies
    #
    # This is the canonical orientation used throughout the package.
    # ------------------------------------------------------------

    validate_matrix_pair <- function(
        plus,
        minus,
        name
    ) {
        if (!is.matrix(plus) || !is.matrix(minus)) {
            stop(
                name,
                " positive and negative objects must be matrices.",
                call. = FALSE
            )
        }

        if (!is.numeric(plus) || !is.numeric(minus)) {
            stop(
                name,
                " matrices must be numeric.",
                call. = FALSE
            )
        }

        if (!identical(dim(plus), dim(minus))) {
            stop(
                name,
                " positive and negative matrices must have identical dimensions.",
                call. = FALSE
            )
        }

        if (is.null(rownames(plus)) ||
            is.null(colnames(plus)) ||
            is.null(rownames(minus)) ||
            is.null(colnames(minus))) {
            stop(
                name,
                " matrices must have row and column names.",
                call. = FALSE
            )
        }

        if (!identical(rownames(plus), rownames(minus))) {
            stop(
                name,
                " positive and negative matrices must have identical row names.",
                call. = FALSE
            )
        }

        if (!identical(colnames(plus), colnames(minus))) {
            stop(
                name,
                " positive and negative matrices must have identical column names.",
                call. = FALSE
            )
        }
    }

    validate_matrix_pair(
        M_plus,
        M_minus,
        "M"
    )

    # ------------------------------------------------------------
    # Check dimensions against the conceptual structure
    #
    # S = issues
    # E = policies
    # M = issues x policies
    # ------------------------------------------------------------

    if (nrow(M_plus) != length(S_plus)) {
        stop(
            "M must have one row for every issue in S.",
            call. = FALSE
        )
    }

    if (ncol(M_plus) != length(E_plus)) {
        stop(
            "M must have one column for every policy in E.",
            call. = FALSE
        )
    }

    # ------------------------------------------------------------
    # Raw positive / negative structures
    # ------------------------------------------------------------

    plus <- list(
        S = S_plus,
        M = M_plus,
        E = E_plus
    )

    minus <- list(
        S = S_minus,
        M = M_minus,
        E = E_minus
    )

    # ------------------------------------------------------------
    # Derived measures
    # ------------------------------------------------------------

    measures <- list(
        positive = plus,
        negative = minus,

        net = list(
            S = plus$S - minus$S,
            M = plus$M - minus$M,
            E = plus$E - minus$E
        ),

        gross = list(
            S = plus$S + minus$S,
            M = plus$M + minus$M,
            E = plus$E + minus$E
        )
    )

    # ------------------------------------------------------------
    # Derive all secondary structures independently
    # ------------------------------------------------------------

    derived <- lapply(
        measures,
        derive_claim_space,
        svd_tol = svd_tol
    )

    # ------------------------------------------------------------
    # Object
    # ------------------------------------------------------------

    structure(
        list(
            measures = measures,
            derived = derived,
            issues = issues,
            policies = policies
        ),
        class = "claim_space"
    )
}


# ================================================================
# Derived claim-space quantities
# ================================================================

derive_claim_space <- function(
    x,
    svd_tol = 1e-10
) {
    S <- x$S
    M <- x$M
    E <- x$E

    # ------------------------------------------------------------
    # E*
    #
    # M is issues x policies.
    #
    # Sum across issues for each policy.
    # ------------------------------------------------------------

    E_star_raw <- drop(
        t(M) %*% rep(1, nrow(M))
    )

    E_star <- normalise_to(
        E_star_raw,
        E
    )

    # ------------------------------------------------------------
    # S*
    #
    # Sum across policies for each issue.
    # ------------------------------------------------------------

    S_star_raw <- drop(
        M %*% rep(1, ncol(M))
    )

    S_star <- normalise_to(
        S_star_raw,
        S
    )

    # ------------------------------------------------------------
    # E^
    #
    # Sum issue salience weighted by impact for each policy.
    # ------------------------------------------------------------

    E_hat_raw <- drop(
        t(M) %*% S
    )

    E_hat <- normalise_to(
        E_hat_raw,
        E
    )

    # ------------------------------------------------------------
    # S^
    #
    # Sum policy evaluation weighted by impact for each issue.
    # ------------------------------------------------------------

    S_hat_raw <- drop(
        M %*% E
    )

    S_hat <- normalise_to(
        S_hat_raw,
        S
    )

    list(
        `E*` = E_star,
        `S*` = S_star,
        `E^` = E_hat,
        `S^` = S_hat
    )
}


# ================================================================
# Normalise vector to the norm of a reference vector
# ================================================================

normalise_to <- function(
    x,
    reference
) {
    x_norm <- sqrt(
        sum(x^2)
    )

    reference_norm <- sqrt(
        sum(reference^2)
    )

    if (
        x_norm == 0 ||
        reference_norm == 0
    ) {
        return(
            stats::setNames(
                rep(0, length(x)),
                names(x)
            )
        )
    }

    result <- (
        x / x_norm
    ) * reference_norm

    stats::setNames(
        result,
        names(x)
    )
}


# ================================================================
# Moore-Penrose pseudoinverse
# ================================================================

pinv <- function(
    x,
    tol = 1e-10
) {
    s <- svd(x)

    if (!length(s$d)) {
        return(
            matrix(
                0,
                nrow = ncol(x),
                ncol = nrow(x)
            )
        )
    }

    threshold <- max(s$d) * tol

    d_inv <- ifelse(
        s$d > threshold,
        1 / s$d,
        0
    )

    sweep(
        s$v,
        2,
        d_inv,
        `*`
    ) %*%
        t(s$u)
}


# ================================================================
# Print method
# ================================================================

print.claim_space <- function(
    x,
    ...
) {
    cat("<claim_space>\n")
    cat(
        "Issues:",
        length(x$issues),
        "\n"
    )
    cat(
        "Policies:",
        length(x$policies),
        "\n"
    )

    invisible(x)
}


# ================================================================
# Summary method
# ================================================================

summary.claim_space <- function(
    object,
    ...
) {
    object
}

