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
    ind$add_decision_rule()

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
    expect_true(
      any(
        class(ind$decision_rule$noise) == "quosure"
      )
    )
    expect_true(
      any(
        class(ind$decision_rule$formula) == "quosure"
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
    ind$add_decision_rule(
      formula = x ~ y,
      noise = rnorm()
    )

    # Test
    expect_true(
      all(
        class(ind$decision_rule) == c("decision_rule", "R6")
      )
    )
    expect_true(
      any(
        class(ind$decision_rule$noise) == "quosure"
      )
    )
    expect_identical(
      get_expr(ind$decision_rule$noise),
      get_expr(quo(rnorm()))
    )
    expect_true(
      any(
        class(ind$decision_rule$formula) == "quosure"
      )
    )
    expect_identical(
      get_expr(ind$decision_rule$formula),
      get_expr(quo(x ~ y))
    )
  }
)