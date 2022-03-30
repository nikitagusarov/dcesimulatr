############################
# Test designs_fractioning #
############################

test_that(
  "designs_f works",
  {
    # Init experimental design
    # Generate dummy alternative
    alt1 <- alternative$new()
    alt1$add_attributes(
      Price = c(1:3),
      Quality = c(0:1)
    )
    alt2 <- alternative$new()
    alt2$add_attributes(
      Opinion = c(0:1),
      Quality = c(0:1)
    )
    e_design <- experimental_design$new(
      list(alt1, alt2),
      n = 24
    )

    # Generate using factors
    Z <- designs_f(e_design)
    Z <- designs_fractioning(Z)

    # Tests
    expect_true(
      class(Z) == "data.frame"
    )
    expect_true(
      all(dim(Z) == c(24, 5))
    )
  }
)