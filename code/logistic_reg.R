## Generalized Linear Models
## Logistic Regression

library(tidyverse)
library(sjPlot)

nhanesdata2 = nhanesdata2 %>%
  mutate(diabetes = relevel(diabetes, ref = "No"))

# Crude bivariate model (equivalent to contingency table)
g1 = glm(depressed ~ diabetes, family = binomial(), data = nhanesdata2)
summary(g1)

# coefficient = log(OR)

# exponentiate to obtain OR
exp(coef(g1))
exp(confint(g1)) # confidence interval

# For a model with categorical predictor
g2 = glm(depressed ~ bmi_cat, family = binomial(), data = nhanesdata2)
summary(g2)
exp(coef(g2))
exp(confint(g2))
# Based on results, it's best to relevel bmi_cat so that the ref is "24.6-29.69"

# Continuous predictor (A1c)
g3 = glm(depressed ~ LBXGH, family = binomial(), data = nhanesdata2)
summary(g3)
exp(coef(g3))
exp(confint(g3))

# Multivariate/adjusted model; bmi and age are covariates
g4 = glm(depressed ~ diabetes + bmi_cat + RIDAGEYR, family = binomial(), data = nhanesdata2)
summary(g4)
exp(coef(g4))

# sjPlot package for plotting and tabulating results

# plot_model produces a ggplot object, which can additional layers added to it
plot_model(g4) + theme_classic()
plot_model(g4, type = "pred", terms = "diabetes") + theme_classic() +
  ylab("Depressed Prevalence") + labs(title = "Prevalence of Depression vs Diabetes")

# plot_models can be used to plot multiple models
# This displays a forest plot with both sets of OR's on the same plot
plot_models(g1, g4)


# tab_model produces a table display of one or more model results
# use file= to save results in a file; use .doc extension to coerce into Word document
tab_model(g1, g2, g3, g4, file = "dep_diab_tabs.doc")
