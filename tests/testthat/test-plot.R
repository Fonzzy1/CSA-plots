library(testthat)
library(claimspace)

# -------------------------------------------------------------------------
# Shared example claim space
#
# Canonical orientation:
#
#   S = issues
#   E = policies
#   M = issues x policies
#
#                 Renewables   Gas
#   Energy              -12     15
#   Environment          12      8
# -------------------------------------------------------------------------

S_plus <- c(
  Energy = 20,
  Environment = 3
)

S_minus <- c(
  Energy = 0,
  Environment = 0
)

M_plus <- matrix(
  c(
    0, 15,
    12, 8
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    c("Energy", "Environment"),
    c("Renewables", "Gas")
  )
)

M_minus <- matrix(
  c(
    12, 0,
    0, 0
  ),
  nrow = 2,
  byrow = TRUE,
  dimnames = list(
    c("Energy", "Environment"),
    c("Renewables", "Gas")
  )
)

E_plus <- c(
  Renewables = 23,
  Gas = 8
)

E_minus <- c(
  Renewables = 38,
  Gas = 0
)

cs <- claim_space(
  S_plus = S_plus,
  S_minus = S_minus,
  M_plus = M_plus,
  M_minus = M_minus,
  E_plus = E_plus,
  E_minus = E_minus
)


# -------------------------------------------------------------------------
# CSA orientation
# -------------------------------------------------------------------------

test_that("claim space uses issues x policies orientation for M", {
  M <- cs$measures$net$M

  expect_equal(
    rownames(M),
    c("Energy", "Environment")
  )

  expect_equal(
    colnames(M),
    c("Renewables", "Gas")
  )

  expect_equal(
    dim(M),
    c(2, 2)
  )

  expect_equal(
    cs$measures$net$S,
    c(
      Energy = 20,
      Environment = 3
    )
  )

  expect_equal(
    cs$measures$net$E,
    c(
      Renewables = -15,
      Gas = 8
    )
  )
})


# -------------------------------------------------------------------------
# Derived structures
# -------------------------------------------------------------------------

test_that("derived structures have the correct dimensions", {
  expect_length(
    cs$derived$net$`S*`,
    length(S_plus)
  )

  expect_length(
    cs$derived$net$`S^`,
    length(S_plus)
  )

  expect_length(
    cs$derived$net$`E*`,
    length(E_plus)
  )

  expect_length(
    cs$derived$net$`E^`,
    length(E_plus)
  )

  expect_named(
    cs$derived$net$`S*`,
    names(S_plus)
  )

  expect_named(
    cs$derived$net$`S^`,
    names(S_plus)
  )

  expect_named(
    cs$derived$net$`E*`,
    names(E_plus)
  )

  expect_named(
    cs$derived$net$`E^`,
    names(E_plus)
  )
})


# -------------------------------------------------------------------------
# Full figure
#
# Corresponds to the manuscript's:
#
#   S, S^, S*, M
#   E, E^, E*
#
# M is always present.
# -------------------------------------------------------------------------

test_that("full claim-space plot matches the manuscript structure", {
  p <- plot_claims(
    cs,
    s = c("S", "S^", "S*"),
    e = c("E", "E^", "E*"),
    measure = "net"
  )

  expect_s3_class(
    p,
    "ggplot"
  )

  # Make sure the plot can actually be rendered.
  expect_no_error(
    print(p)
  )
})


# -------------------------------------------------------------------------
# Observed framing figure
#
# Corresponds to:
#
#   S, M
#   E
# -------------------------------------------------------------------------

test_that("observed framing plot works", {
  p <- plot_claims(
    cs,
    s = "S",
    e = "E",
    measure = "net"
  )

  expect_s3_class(
    p,
    "ggplot"
  )

  expect_no_error(
    print(p)
  )
})


# -------------------------------------------------------------------------
# Implied evaluation figure
#
# Corresponds to:
#
#   S, M
#   E^
# -------------------------------------------------------------------------

test_that("implied evaluation plot works", {
  p <- plot_claims(
    cs,
    s = "S",
    e = "E^",
    measure = "net"
  )

  expect_s3_class(
    p,
    "ggplot"
  )

  expect_no_error(
    print(p)
  )
})


# -------------------------------------------------------------------------
# Implied salience / equalised comparison
#
# Corresponds to:
#
#   S, S^, S*, M
#   E, E^, E*
# -------------------------------------------------------------------------

test_that("implied salience comparison plot works", {
  p <- plot_claims(
    cs,
    s = c("S", "S^", "S*"),
    e = c("E", "E^", "E*"),
    measure = "net"
  )

  expect_s3_class(
    p,
    "ggplot"
  )

  expect_no_error(
    print(p)
  )
})


# -------------------------------------------------------------------------
# Individual derived structures
# -------------------------------------------------------------------------

test_that("individual S and E structures can be plotted", {
  expect_s3_class(
    plot_claims(
      cs,
      s = "S^",
      e = "E",
      measure = "net"
    ),
    "ggplot"
  )

  expect_s3_class(
    plot_claims(
      cs,
      s = "S*",
      e = "E^",
      measure = "net"
    ),
    "ggplot"
  )
})


# -------------------------------------------------------------------------
# Different measures
# -------------------------------------------------------------------------

test_that("plots work for all claim measures", {
  for (measure in c(
    "positive",
    "negative",
    "net",
    "gross"
  )) {
    p <- plot_claims(
      cs,
      s = c("S", "S^", "S*"),
      e = c("E", "E^", "E*"),
      measure = measure
    )

    expect_s3_class(
      p,
      "ggplot"
    )

    expect_no_error(
      print(p)
    )
  }
})
