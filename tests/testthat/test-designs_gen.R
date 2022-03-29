####################
# Test designs_gen #
####################

test_that(
  "designs_gen works: only factors",
  {
    # Init experimental design
    # Generate dummy alternative
    alt1 <- alternative$new()
    alt1$add_attributes(
      Price = c(1:2),
      Quality = c(0:1)
    )
    alt2 <- alternative$new()
    alt2$add_attributes(
      Opinion = c(0:1),
      Quality = c(0:1)
    )
    e_design <- experimental_design$new(
      list(alt1, alt2),
      n = 3
    )

    # Generate using factors
    Z <- designs_gen(e_design)

    # Test
    expect_true(
      class(Z) == "data.frame"
    )
    expect_true(
      all(dim(Z) == c(6, 5))
    )
    expect_true(
      sum(is.na(Z)) == 6
    )
  }
)

test_that(
  "designs_gen works: only random generator",
  {
    # Init experimental design
    # Generate dummy alternative
    alt1 <- alternative$new()
    alt1$add_attributes(
      Price = rnorm(sd = 10),
      Quality = rexp(rate = 2)
    )
    alt2 <- alternative$new()
    alt2$add_attributes(
      Opinion = rnorm(mean = 10),
      Quality = rexp(rate = 5)
    )
    e_design <- experimental_design$new(
      list(alt1, alt2),
      n = 3
    )

    # Generate using random generator
    Z <- designs_gen(e_design)

    # Test
    expect_true(
      class(Z) == "data.frame"
    )
    expect_true(
      all(dim(Z) == c(6, 5))
    )
    expect_true(
      sum(is.na(Z)) == 6
    )
  }
)

test_that(
  "designs_gen works: factors and random (1)",
  {
    # Init experimental design
    # Generate dummy alternative
    alt1 <- alternative$new()
    alt1$add_attributes(
      Price = c(1:2),
      Quality = rexp(rate = 2)
    )
    alt2 <- alternative$new()
    alt2$add_attributes(
      Opinion = rnorm(mean = 10),
      Quality = c(0:1)
    )
    e_design <- experimental_design$new(
      list(alt1, alt2),
      n = 3
    )

    # Generate using factors
    Z <- designs_gen(e_design)

    # Test
    expect_true(
      class(Z) == "data.frame"
    )
    expect_true(
      all(dim(Z) == c(6, 5))
    )
    expect_true(
      sum(is.na(Z)) == 6
    )
  }
)

test_that(
  "designs_gen works: factors and random (2)",
  {
    # Init filled
    # Generate dummy alternative
    alt1 <- alternative$new()
    alt1$add_attributes(
      Price = c(1:4)
    )
    alt2 <- alternative$new()
    alt2$add_attributes(
      Opinion = c(0:1),
      Quality = rnorm(mean = 1, sd = 3)
    )
    # Fill with corresponding alternative
    e_design <- experimental_design$new(
      list(alt1, alt2),
      # list(alt1, alt2),
      n = 10
    )

    # Generate using factors
    Z <- designs_gen(e_design)

    # Test
    expect_true(
      class(Z) == "data.frame"
    )
    expect_true(
      all(dim(Z) == c(20, 5))
    )
    expect_true(
      sum(is.na(Z)) == 30
    )
  }
)
