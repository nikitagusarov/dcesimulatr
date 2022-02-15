#######################
# Test decision rules #
#######################

# General object test
test_that(
  "object is generated", 
  {
    # Init
    dr = decision_rule$new()

    # Test
    expect_true(
      all(
        class(dr) == c("decision_rule", "R6")
      )
    )
    expect_true(
      any(
        class(dr$noise) == "quosure"
      )
    )
    expect_true(
      any(
        class(dr$formula) == "quosure"
      )
    )
  }
)

# Noise modification in place check
test_that(
  "formula is modified", 
  {
    dr = decision_rule$new()
    dr = dr$modify_noise(
      new_noise = rnorm(mean = 1, sd = 10)
    )

    # Test
    expect_true(
      all(
        class(dr) == c("decision_rule", "R6")
      )
    )
    expect_true(
      any(
        class(dr$noise) == "quosure"
      )
    )
  }
)

# Formula modification in place check
test_that(
  "formula is modified", 
  {
    dr = decision_rule$new()
    dr = dr$modify_formula(
      new_formula = x ~ y
    )

    # Test
    expect_true(
      all(
        class(dr) == c("decision_rule", "R6")
      )
    )
    expect_true(
      any(
        class(dr$formula) == "quosure"
      )
    )
    expect_identical(
      get_expr(dr$formula),
      get_expr(quo(x ~ y))
    )
  }
)
