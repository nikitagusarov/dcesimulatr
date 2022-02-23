##############################
# 1. Test "population" class #
##############################

# Test generation
test_that(
  "object creation", 
  {
    # Init empty
    pop = population$new()

    # Test
    expect_true(
      all(
        class(pop) == c("population", "R6")
      )
    )

    # Init filled
    # Generate dummy ind
    ind1 = individual$new()
    ind1$add_characteristics(
      Age = rnorm(sd = 10)
    )
    ind2 = individual$new()
    ind2$add_characteristics(
      Income = rexp(sd = 10)
    )
    # Fill with corresponding ind
    pop = population$new(
      list(ind1), n = 10
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
    pop = population$new(
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
    ind1 = individual$new()
    ind1$add_characteristics(
      Age = rnorm(sd = 10)
    )
    ind2 = individual$new()
    ind2$add_characteristics(
      Income = rexp(sd = 10)
    )

    # Init non-empty
    pop = population$new(
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
      ind2, 15, profile_name = "np"
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
    ind1 = individual$new()
    ind1$add_characteristics(
      Age = rnorm(sd = 10),
      Income = rnorm(mean = 1, sd = 5)
    )
    ind1$add_decision_rule(
      formula = 1.5*Age + 0.7*Quality
    )
    ind2 = individual$new()
    ind2$add_characteristics(
      Income = rexp(rate = 10)
    )
    ind2$add_decision_rule(
      formula = Age + 2*Income - 0.5*Quality
    )

    # Init non-empty
    pop = population$new(
      list(ind1, ind2), 
      n = list(10, 15)
    )

    # Get characteritics
    chars = pop$get_chars()

    # Test
    expect_true(
      all(
        class(chars) == "character",
        chars == c("Age", "Income")
      )
    )

    # Get n
    n = pop$get_n()

    # Test
    expect_true(
      all(
        class(n) == "numeric",
        n == c(10, 15)
      )
    )

    # Get n
    rules = pop$get_rules()

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
    ind1 = individual$new()
    ind1$add_characteristics(
      Age = rnorm(sd = 10)
    )
    ind1$add_characteristics(
      Income = rnorm(mean = 1, sd = 5)
    )
    ind2 = individual$new()
    ind2$add_characteristics(
      Income = rexp(rate = 10)
    )

    # Init non-empty
    pop = population$new(
      list(ind1, ind2), 
      n = list(10, 15)
    )
    # Generate X
    X = population_gen(pop)

    # Test
    expect_true(
      class(X) == "data.frame"
    )
    expect_true(
      all(colnames(X) == c("Age", "Income", "class"))
    )
    expect_true(
      all(dim(X) == c(25, 3))
    )
  }
)
