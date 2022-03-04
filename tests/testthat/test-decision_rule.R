#######################
# Test decision rules #
#######################

# General object test
test_that(
  "object is generated",
  {
    # Init
    dr <- decision_rule$new()

    # Test
    expect_true(
      all(
        class(dr) == c("decision_rule", "R6")
      )
    )
    expect_true(
      any(
        class(dr$noise) == "list"
      )
    )
    expect_true(
      any(
        class(dr$formula) == "list"
      )
    )
    expect_true(
      class(dr$transformation) == "call"
    )
    expect_true(
      class(dr$choice) == "call"
    )
  }
)

# Noise modification in place check
test_that(
  "formula is modified",
  {
    dr <- decision_rule$new()
    dr <- dr$add_noise(
      rnorm(mean = 0, sd = 1)
    )

    # Test
    expect_true(
      all(
        class(dr) == c("decision_rule", "R6")
      )
    )
    expect_true(
      class(dr$noise) == "list"
    )
    expect_true(
      any(
        class(dr$noise[[1]]) == "call"
      )
    )
  }
)

# Formula modification in place check
test_that(
  "formula is modified",
  {
    dr <- decision_rule$new()
    dr <- dr$add_formulas(
      x + y, x + 2 * z
    )

    # Test
    expect_true(
      all(
        class(dr) == c("decision_rule", "R6")
      )
    )
    expect_true(
      any(
        class(dr$formula) == "list"
      )
    )
    expect_true(
      any(
        class(dr$formula[[1]]) == "call"
      )
    )
    expect_identical(
      get_expr(dr$formula[[1]]),
      get_expr(quo(x + y))
    )
    expect_identical(
      get_expr(dr$formula[[2]]),
      get_expr(quo(x + 2 * z))
    )
  }
)
