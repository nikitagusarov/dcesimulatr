#####################################
# 2. Test "experiment_run" function #
#####################################



# Test generation
test_that(
    "experiment is executed and completed",
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
            Quality = rexp(rate = 2)
        )
        alt2 = alternative$new()
        alt2$add_attributes(
            Opinion = rnorm(mean = 10), 
            Quality = rexp(rate = 5)
        )
        e_design = experimental_design$new(
            list(alt1, alt2), n = 3
        )

        # Call generation function
        res_fin = experiment(
            population = pop,
            experimental_design = e_design
        )

        expect_true(
            class(res_fin) == "data.frame"
        )
        expect_true(
            all(dim(res_fin) == c(150, 13))
        )
        expect_true(
            length(unique(res_fin$CID)) == 
            sum(res_fin$CH)
        )
    }
)