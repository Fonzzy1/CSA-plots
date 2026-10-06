library(testthat)
library(claimspace)


# -------------------------------------------------------------------------
# Basic algebra
#
# Canonical orientation:
#
#   S = issues
#   E = policies
#   M = issues x policies
# -------------------------------------------------------------------------

test_that(
    "claim space calculates the basic algebra",
    {
        S_plus <- c(
            a = 3,
            b = 1
        )

        S_minus <- c(
            a = 1,
            b = 2
        )

        M_plus <- matrix(
            c(
                2, 1,
                1, 3
            ),
            nrow = 2,
            byrow = TRUE,
            dimnames = list(
                c("a", "b"),
                c("p1", "p2")
            )
        )

        M_minus <- matrix(
            c(
                1, 0,
                0, 1
            ),
            nrow = 2,
            byrow = TRUE,
            dimnames = list(
                c("a", "b"),
                c("p1", "p2")
            )
        )

        E_plus <- c(
            p1 = 4,
            p2 = 3
        )

        E_minus <- c(
            p1 = 1,
            p2 = 2
        )

        cs <- claim_space(
            S_plus,
            S_minus,
            M_plus,
            M_minus,
            E_plus,
            E_minus
        )

        expect_equal(
            claim_group(
                cs,
                "S",
                "net"
            ),
            c(
                a = 2,
                b = -1
            )
        )

        expect_equal(
            claim_group(
                cs,
                "E",
                "net"
            ),
            c(
                p1 = 3,
                p2 = 1
            )
        )

        expect_equal(
            claim_group(
                cs,
                "M",
                "net"
            ),
            matrix(
                c(
                    1, 1,
                    1, 2
                ),
                nrow = 2,
                byrow = TRUE,
                dimnames = list(
                    c("a", "b"),
                    c("p1", "p2")
                )
            )
        )
    }
)


# -------------------------------------------------------------------------
# Derived quantities
# -------------------------------------------------------------------------

test_that(
    "derived quantities are calculated",
    {
        cs <- claim_space(
            c(
                a = 1,
                b = 2
            ),

            c(
                a = 0,
                b = 0
            ),

            matrix(
                c(
                    1, 0,
                    0, 1
                ),
                nrow = 2,
                byrow = TRUE,
                dimnames = list(
                    c("a", "b"),
                    c("p1", "p2")
                )
            ),

            matrix(
                0,
                nrow = 2,
                ncol = 2,
                dimnames = list(
                    c("a", "b"),
                    c("p1", "p2")
                )
            ),

            c(
                p1 = 1,
                p2 = 2
            ),

            c(
                p1 = 0,
                p2 = 0
            )
        )

        # E*:
        #
        # column sums of M = (1, 1)
        # normalised to ||E|| = sqrt(5)
        #
        # => (sqrt(5/2), sqrt(5/2))

        expect_equal(
            claim_group(
                cs,
                "E*"
            ),
            c(
                p1 = sqrt(5 / 2),
                p2 = sqrt(5 / 2)
            )
        )

        # E^:
        #
        # t(M) %*% S = (1, 2)
        # This already has the same norm as E.
        #
        # => (1, 2)

        expect_equal(
            claim_group(
                cs,
                "E^"
            ),
            c(
                p1 = 1,
                p2 = 2
            )
        )

        # S*:
        #
        # row sums of M = (1, 1)
        # normalised to ||S|| = sqrt(5)
        #
        # => (sqrt(5/2), sqrt(5/2))

        expect_equal(
            claim_group(
                cs,
                "S*"
            ),
            c(
                a = sqrt(5 / 2),
                b = sqrt(5 / 2)
            )
        )

        # S^:
        #
        # M %*% E = (1, 2)
        # This already has the same norm as S.
        #
        # => (1, 2)

        expect_equal(
            claim_group(
                cs,
                "S^"
            ),
            c(
                a = 1,
                b = 2
            )
        )
    }
)


# -------------------------------------------------------------------------
# Positive, negative, net and gross
# -------------------------------------------------------------------------

test_that(
    "positive, negative, net and gross are available",
    {
        cs <- claim_space(
            c(
                a = 3
            ),

            c(
                a = 1
            ),

            matrix(
                2,
                nrow = 1,
                ncol = 1,
                dimnames = list(
                    "a",
                    "p"
                )
            ),

            matrix(
                1,
                nrow = 1,
                ncol = 1,
                dimnames = list(
                    "a",
                    "p"
                )
            ),

            c(
                p = 4
            ),

            c(
                p = 2
            )
        )

        expect_equal(
            claim_group(
                cs,
                "S",
                "positive"
            ),
            c(
                a = 3
            )
        )

        expect_equal(
            claim_group(
                cs,
                "S",
                "negative"
            ),
            c(
                a = 1
            )
        )

        expect_equal(
            claim_group(
                cs,
                "S",
                "net"
            ),
            c(
                a = 2
            )
        )

        expect_equal(
            claim_group(
                cs,
                "S",
                "gross"
            ),
            c(
                a = 4
            )
        )
    }
)


# -------------------------------------------------------------------------
# Explicit names
# -------------------------------------------------------------------------

test_that(
    "names can be supplied explicitly",
    {
        cs <- claim_space(
            c(1, 2),
            c(0, 0),

            matrix(
                1:4,
                nrow = 2,
                ncol = 2
            ),

            matrix(
                0,
                nrow = 2,
                ncol = 2
            ),

            c(1, 2),
            c(0, 0),

            issues = c(
                "issue_1",
                "issue_2"
            ),

            policies = c(
                "policy_1",
                "policy_2"
            )
        )

        expect_equal(
            cs$issues,
            c(
                "issue_1",
                "issue_2"
            )
        )

        expect_equal(
            cs$policies,
            c(
                "policy_1",
                "policy_2"
            )
        )

        expect_equal(
            rownames(
                claim_group(
                    cs,
                    "M"
                )
            ),
            c(
                "issue_1",
                "issue_2"
            )
        )

        expect_equal(
            colnames(
                claim_group(
                    cs,
                    "M"
                )
            ),
            c(
                "policy_1",
                "policy_2"
            )
        )
    }
)
