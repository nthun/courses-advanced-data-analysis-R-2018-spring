# Confirmatory factor analysis using the lavaan package
# Check out the hompage: http://lavaan.ugent.be/

install.packages("lavaan")


library(tidyverse)
library(lavaan)

hs_data <- 
    lavaan::HolzingerSwineford1939 %>% 
    as_tibble()

# Defining the CFA model
hs_model <- " visual  =~ x1 + x2 + x3 
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9
            "

fit <- cfa(hs_model, data = hs_data)

# If you want to get the fit measures, you have to set this parameter in summary()
# This returns the most important information about the model
summary(fit, fit.measures = TRUE)

# Plotting the models is easy with semPlot, but parameters require tweaking to make it informative and pretty
install.packages("semPlot")
library(semPlot)

# This will give you the clean model structure
semPaths(fit)
# If you want to get the parameter estimates use
semPaths(fit, "est")

# You can also get standardized estimates
semPaths(fit, "std")

# This does not tell us much

# Fixing intercepts
# "
    # # three-factor model
    # visual =~ x1 + x2 + x3
    # textual =~ x4 + x5 + x6
    # speed   =~ x7 + x8 + x9
    # 
    # # intercepts
    # x1 ~ 1
    # x2 ~ 1
    # x3 ~ 1
    # x4 ~ 1
    # x5 ~ 1
    # x6 ~ 1
    # x7 ~ 1
    # x8 ~ 1
    # x9 ~ 1
# "

# Lets define the model in a separate file, and read it (this may be a good way to keep your model)

hs_model_fi <- read_lines("CFA models/fs_model_fi.txt")

fit2 <- cfa(hs_model_fi, hs_data)
summary(fit2, fit.measures = TRUE)

hs_model_ort <- read_lines("CFA models/hs_model_orthogonal.txt")

fit3 <- cfa(hs_model_ort, hs_data)
summary(fit3, fit.measures = TRUE)
semPaths(fit3, "est")


# We can also regard all latent variables to be independent (orthogonal) by using the orthogonal = TRUE parameter on our original model

fit4 <- cfa(hs_model, data = hs_data, orthogonal = TRUE)
summary(fit4, fit.measures = TRUE)
semPaths(fit4, "est")

# If the factor loadings of the first indicator of each latent variable will no longer be fixed to 1 if you use the std.lev = TRUE parameter
fit5 <- cfa(hs_model, hs_data, std.lev = TRUE)
summary(fit5, fit.measures = TRUE)
semPaths(fit5, "est")

# You can build 2-level structurese (see file)
hs_model_2fac <- read_lines("CFA models/fs_model_2fac.txt")

fit6 <- cfa(hs_model_2fac, hs_data, std.lev = TRUE)
summary(fit6, fit.measures = TRUE)
semPaths(fit6, "est")

# You can do multi group analysis

fit7 <- cfa(hs_model, data = hs_data, group = "school")
summary(fit7, fit.measures = TRUE)
semPaths(fit7, "est")



