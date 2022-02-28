#########################################
# 1. Test "experiment_compose" function #
#########################################



# Test generation
test_that(
    "generation function works (default params)",
    {
        # Init Population
        # Generate dummy ind 1
        ind1 = individual$new()
        ind1$add_characteristics(
            Age = rnorm(sd = 10),
            Income = rnorm(mean = 1, sd = 5)
        )
        dr1 = decision_rule$new()
        dr1 = dr1$add_noise(
            rnorm(sd = 2)
        )
        dr1 = dr1$add_formulas(
            1.5*Age + Quality, 
            Age + Quality^2
        )
        ind1$add_decision_rule(dr1)
        # Dummy ind2
        ind2 = individual$new()
        ind2$add_characteristics(
            Income = rexp(rate = 10)
        )
        dr2 = decision_rule$new()
        dr2 = dr2$add_noise(
            rexp(rate = 1)
        )
        dr2 = dr2$add_formulas(
            2*Income, 
            Income - 0.5*Quality
        )
        ind2$add_decision_rule(dr2)
        
        pop = population$new(
            list(ind1, ind2), 
            n = list(10, 15)
        )

        # Init experimental design
        # Generate dummy alternative
        alt1 = alternative$new()
        alt1$add_attributes(
            Price = rnorm(sd = 10),
            Quality = rexp()
        )
        alt2 = alternative$new()
        alt2$add_attributes(
            Opinion = rnorm(mean = 10), 
            Quality = rexp(rate = 5)
        )
        e_design = experimental_design$new(
            list(alt1, alt2)
        )

        # Call generation function
        exp_res = experiment_compose(
            population = pop,
            experimental_design = e_design
        )

        expect_true(
            class(exp_res) == "data.frame"
        )
        expect_true(
            all(dim(exp_res) == c(50, 9))
        )
    }
)

# Test generation
test_that(
    "generation function works (non-default params)",
    {
        # Init Population
        # Generate dummy ind 1
        ind1 = individual$new()
        ind1$add_characteristics(
            Age = rnorm(sd = 10),
            Income = rnorm(mean = 1, sd = 5)
        )
        dr1 = decision_rule$new()
        dr1 = dr1$add_noise(
            rnorm(sd = 2)
        )
        dr1 = dr1$add_formulas(
            1.5*Age + Quality, 
            Age + Quality^2
        )
        ind1$add_decision_rule(dr1)
        # Dummy ind2
        ind2 = individual$new()
        ind2$add_characteristics(
            Income = rexp(rate = 10)
        )
        dr2 = decision_rule$new()
        dr2 = dr2$add_noise(
            rexp(rate = 1)
        )
        dr2 = dr2$add_formulas(
            2*Income, 
            Income - 0.5*Quality
        )
        ind2$add_decision_rule(dr2)
        
        pop = population$new(
            list(ind1, ind2), 
            n = list(10, 15)
        )

        # Init experimental design
        # Generate dummy alternative
        alt1 = alternative$new()
        alt1$add_attributes(
            Price = rnorm(sd = 10)
        )
        alt2 = alternative$new()
        alt2$add_attributes(
            Opinion = rnorm(mean = 10), 
            Quality = rexp(rate = 5)
        )
        e_design = experimental_design$new(
            list(alt1, alt2), n = 3, identical = FALSE
        )

        # Call generation function
        exp_res = experiment_compose(
            population = pop,
            experimental_design = e_design
        )

        attr = e_design$get_attributes()
        expect_true(
            class(exp_res) == "data.frame"
        )
        expect_true(
            all(dim(exp_res) == c(150, 9))
        )
        expect_true(
            sum(exp_res[1:6, attr] == exp_res[7:12, attr], na.rm = TRUE) == 0
        )

        # Identical = FALSE
        e_design = experimental_design$new(
            list(alt1, alt2), n = 3, identical = TRUE
        )

        # Call generation function
        exp_res = experiment_compose(
            population = pop,
            experimental_design = e_design
        )

        expect_true(
            class(exp_res) == "data.frame"
        )
        expect_true(
            all(dim(exp_res) == c(150, 9))
        )
        expect_true(
            sum(exp_res[1:6, attr] == exp_res[7:12, attr], na.rm = TRUE) == 9
        )
    }
)
