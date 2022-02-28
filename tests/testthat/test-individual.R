####################
# Test individuals #
####################

# Test generation
test_that(
  "object creation", 
  {
    # Init
    ind = individual$new()

    # Test
    expect_true(
      all(
        class(ind) == c("individual", "R6")
      )
    )
  }
)

# Test
test_that(
  "adding characteristics", 
  {
    # Init
    ind = individual$new()
    ind$add_characteristics(
      Age = rnorm()
    )

    # Test
    expect_true(
      all(
        class(ind) == c("individual", "R6")
      )
    )
    expect_identical(
      names(ind$characteristics), 
      "Age"
    )
    expect_true(
      any(
        class(ind$characteristics$Age) == "call"
      )
    )

    # Overwrite chars
    ind$add_characteristics(
      Size = rnorm(mean = 10), Sex = rexp(scale = 5)
    )

    # Test
    expect_identical(
      names(ind$characteristics), 
      c("Age", "Size", "Sex")
    )
    expect_true(
      any(
        class(ind$characteristics$Sex) == "call"
      )
    )
  }
)

# Test add decision rule simple
test_that(
  "adding decision rules", 
  {
    # Init
    ind = individual$new()
    dr = decision_rule$new()
    ind$add_decision_rule(dr)

    # Test
    expect_true(
      all(
        class(ind) == c("individual", "R6")
      )
    )
    expect_true(
      all(
        class(ind$decision_rule) == c("decision_rule", "R6")
      )
    )
  }
)

# Test add decision rules with assignment
test_that(
  "adding decision rules", 
  {
    # Init
    ind = individual$new()
    dr = decision_rule$new()
    dr = dr$add_noise(
      rnorm()
    )
    dr = dr$add_formulas(
      x + y, x + 2*z
    )

    ind$add_decision_rule(dr)

    # Test
    expect_true(
      all(
        class(ind$decision_rule) == c("decision_rule", "R6")
      )
    )
    expect_true(
      any(
        class(ind$decision_rule$noise) == "list"
      )
    )
    expect_identical(
      get_expr(ind$decision_rule$noise[[1]]),
      get_expr(quo(rnorm()))
    )
    expect_true(
      any(
        class(ind$decision_rule$formula) == "list"
      )
    )
    expect_identical(
      get_expr(ind$decision_rule$formula[[1]]),
      get_expr(quo(x + y))
    )
  }
)