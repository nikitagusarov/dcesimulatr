############################
# 1. Test compose wrappers #
############################



# Fully random specification - identical

test_that(
  "experimental_design generation function test (random)",
  {
    # Init filled
    # Generate dummy alternative
    alt1 <- alternative$new()
    alt1$add_attributes(
      Price = rnorm(sd = 10)
    )
    alt2 <- alternative$new()
    alt2$add_attributes(
      Opinion = rnorm(mean = 10),
      Quality = rexp(rate = 5)
    )
    # Fill with corresponding alternative
    e_design <- experimental_design$new(
      list(alt1, alt2),
      n = 10
    )

    # Generate Z
    Z <- compose_identical(e_design, size = 2)

    # Test
    expect_true(
      class(Z) == "data.frame"
    )
    expect_true(
      all(colnames(Z) == c("Price", "Opinion", "Quality", "AID", "CID"))
    )
    expect_true(
      all(dim(Z) == c(40, 5))
    )
    expect_true(
      sum(Z[1:20, 1:3] != Z[21:40, 1:3], na.rm = TRUE) == 0
    )
  }
)

# Fully random specification - non-identical

test_that(
  "experimental_design generation function test (random)",
  {
    # Init filled
    # Generate dummy alternative
    alt1 <- alternative$new()
    alt1$add_attributes(
      Price = rnorm(sd = 10)
    )
    alt2 <- alternative$new()
    alt2$add_attributes(
      Opinion = rnorm(mean = 10),
      Quality = rexp(rate = 5)
    )
    # Fill with corresponding alternative
    e_design <- experimental_design$new(
      list(alt1, alt2),
      n = 10
    )

    # Generate Z
    Z <- compose_random(e_design, size = 2)

    # Test
    expect_true(
      class(Z) == "data.frame"
    )
    expect_true(
      all(colnames(Z) == c("Price", "Opinion", "Quality", "AID", "CID"))
    )
    expect_true(
      all(dim(Z) == c(40, 5))
    )
    expect_true(
      sum(Z[1:20, 1:3] == Z[21:40, 1:3], na.rm = TRUE) == 0
    )
  }
)
