##################
# Test designs_r #
##################

test_that(
  "designs_r works",
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
    Z <- designs_r(e_design)

    # Test
    expect_true(
      class(Z) == "data.frame"
    )
    expect_true(
      all(dim(Z) == c(6, 5))
    )
  }
)
