##############################
# 1. Test "population" class #
##############################

# Test generation
test_that(
  "object creation",
  {
    # Init empty
    pop <- population$new()

    # Test
    expect_true(
      all(
        class(pop) == c("population", "R6")
      )
    )

    # Init filled
    # Generate dummy ind
    ind1 <- individual$new()
    ind1$add_characteristics(
      Age = rnorm(sd = 10)
    )
    ind2 <- individual$new()
    ind2$add_characteristics(
      Income = rexp(sd = 10)
    )
    # Fill with corresponding ind
    pop <- population$new(
      list(ind1),
      n = 10
    )

    # Test
    expect_true(
      all(
        class(pop) == c("population", "R6")
      )
    )
    expect_true(
      all(
        class(pop$profiles[[1]]) == c("individual", "R6")
      )
    )
    expect_true(
      pop$n[[1]] == 10
    )

    # Fill with corresponding ind
    pop <- population$new(
      profiles = list(ind1, ind2), n = list(10, 15)
    )

    # Test
    expect_true(
      all(
        class(pop) == c("population", "R6")
      )
    )
    expect_true(
      all(
        class(pop$profiles[[1]]) == c("individual", "R6"),
        class(pop$profiles[[2]]) == c("individual", "R6")
      )
    )
    expect_true(
      all(
        pop$n[[1]] == 10,
        pop$n[[2]] == 15
      )
    )
  }
)

# Test adding profiles
test_that(
  "adding profiles",
  {
    # Init filled
    # Generate dummy ind
    ind1 <- individual$new()
    ind1$add_characteristics(
      Age = rnorm(sd = 10)
    )
    ind2 <- individual$new()
    ind2$add_characteristics(
      Income = rexp(sd = 10)
    )

    # Init non-empty
    pop <- population$new(
      list(ind1),
      n = list(10)
    )

    # Test
    expect_true(
      all(
        class(pop) == c("population", "R6")
      )
    )
    expect_true(
      all(
        class(pop$profiles[[1]]) == c("individual", "R6")
      )
    )
    expect_true(
      pop$n[[1]] == 10
    )

    # Fill with corresponding ind
    pop$add_profile(
      ind2, 15,
      profile_name = "np"
    )

    # Test
    expect_true(
      all(
        class(pop) == c("population", "R6")
      )
    )
    expect_true(
      all(
        class(pop$profiles[[1]]) == c("individual", "R6"),
        class(pop$profiles[[2]]) == c("individual", "R6")
      )
    )
    expect_true(
      all(
        pop$n[[1]] == 10,
        pop$n[[2]] == 15
      )
    )
  }
)

# Test querries
test_that(
  "querries test",
  {
    # Init filled
    # Generate dummy ind
    ind1 <- individual$new()
    ind1$add_characteristics(
      Age = rnorm(sd = 10),
      Income = rnorm(mean = 1, sd = 5)
    )

    dr1 <- decision_rule$new()
    dr1 <- dr1$add_noise(
      rnorm(sd = 2)
    )
    dr1 <- dr1$add_formulas(
      1.5 * Age + Quality, Age + Quality^2
    )

    ind1$add_decision_rule(dr1)

    ind2 <- individual$new()
    ind2$add_characteristics(
      Income = rexp(rate = 10)
    )

    dr2 <- decision_rule$new()
    dr2 <- dr2$add_noise(
      rexp(rate = 1)
    )
    dr2 <- dr2$add_formulas(
      Age + 2 * Income, Income - 0.5 * Quality
    )

    ind2$add_decision_rule(dr2)

    # Init non-empty
    pop <- population$new(
      list(ind1, ind2),
      n = list(10, 15)
    )

    # Get characteritics
    chars <- pop$get_chars()

    # Test
    expect_true(
      all(
        class(chars) == "character",
        chars == c("Age", "Income")
      )
    )

    # Get n
    n <- pop$get_n()

    # Test
    expect_true(
      all(
        class(n) == "numeric",
        n == c(10, 15)
      )
    )

    # Get n
    rules <- pop$get_rules()

    # Test
    expect_true(
      class(rules) == "list"
    )
    expect_true(
      any(class(rules[[1]]) == "decision_rule")
    )
  }
)



#####################################################
# 2. Test functions operating in "population" class #
#####################################################

test_that(
  "population generation function test",
  {
    # Init filled
    # Generate dummy ind
    ind1 <- individual$new()
    ind1$add_characteristics(
      Age = rnorm(sd = 10)
    )
    ind1$add_characteristics(
      Income = rnorm(mean = 1, sd = 5)
    )
    ind2 <- individual$new()
    ind2$add_characteristics(
      Income = rexp(rate = 10)
    )

    # Init non-empty
    pop <- population$new(
      list(ind1, ind2),
      n = list(10, 15)
    )
    # Generate X
    X <- population_gen(pop)

    # Test
    expect_true(
      class(X) == "data.frame"
    )
    expect_true(
      all(dim(X) == c(25, 4))
    )
  }
)
