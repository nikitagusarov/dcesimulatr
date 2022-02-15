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



#####################################################
# 2. Test functions operating in "population" class #
#####################################################
