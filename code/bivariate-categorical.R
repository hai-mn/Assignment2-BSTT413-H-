library(tidyverse)
library(janitor)
library(pubh)

# Bivariate Analysis of Categorical Data

## In this section we first investigate the association of two binary variables.

## Cross-tabulation

table(nhanesdata2$depressed, nhanesdata2$diabetes)

# dplyr package
nhanesdata2 %>%
  group_by(depressed, diabetes) %>%
  tally()

# Using the janitor package
nhanesdata2 %>%
  tabyl(diabetes, depressed)

nhanesdata2 %>%
  tabyl(diabetes, depressed)%>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_ns()


## Testing the statistical significance of the association between diabetes and depression
## using chi-squared with (r-1) * (c-1) degrees of freedom

chisq.test(nhanesdata2$diabetes, nhanesdata2$depressed)

qchisq(1-0.05, 1) # critical value

## Measures of association and confidence intervals
nhanesdata2 %>%
  contingency(depressed ~ diabetes)

# relevel to set reference
nhanesdata2 %>%
  mutate(diabetes = relevel(diabetes, ref = "No")) %>%
  contingency(depressed ~ diabetes)


## Binary vs Categorical
nhanesdata2 %>%
  tabyl(bmi_cat, diabetes)%>%
  adorn_totals(c("row", "col")) %>%
  adorn_percentages("row") %>%
  adorn_ns()

## Hypothesis testing
# chi-square with df = (3-1)*(2-1) = 3
chisq.test(nhanesdata2$bmi_cat, nhanesdata2$diabetes)

## Measures of association
## Requires breaking up bmi_cat into multiple groups with the same reference category

nhanesdata2 %>%
  mutate(bmi_cat = factor(bmi_cat, levels =  c("24.6-29.69", "14.2-24.6"))) %>%
  contingency(depressed ~ bmi_cat, method = "case.control")

nhanesdata2 %>%
  mutate(bmi_cat = factor(bmi_cat, levels =  c("24.6-29.69", "29.69-33.5"))) %>%
  contingency(depressed ~ bmi_cat, method = "case.control")

nhanesdata2 %>%
  mutate(bmi_cat = factor(bmi_cat, levels =  c("24.6-29.69", "33.5-86.2"))) %>%
  contingency(depressed ~ bmi_cat, method = "case.control")

