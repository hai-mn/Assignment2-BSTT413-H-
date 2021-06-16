library(tidyverse)

## Numeric vs. Categorical Inference

# The primary objectives are to determine the significance of the associaton between 
# a numeric variable and a categorical variable, and to quantify the strength of the association.

# Example: Is A1c (numeric) associated with depression status (binary)?

nhanesdata2 %>%
  group_by(depressed) %>%
  summarise(mean_a1c = mean(LBXGH, na.rm=T),
            sd_a1c = sd(LBXGH, na.rm = T))

nhanesdata2 %>%
  ggplot() +
  geom_density(aes(x=LBXGH)) +
  facet_wrap(~ depressed) +
  theme_classic()

nhanesdata2 = nhanesdata2 %>%
  mutate(log_A1c = log(LBXGH))

nhanesdata2 %>%
  ggplot() +
  geom_density(aes(x=log_A1c)) +
  facet_wrap(~ depressed) +
  theme_classic()

# t-test (unequal variance)
t.test(LBXGH ~ depressed, data = nhanesdata2)
t.test(log_A1c ~ depressed, data = nhanesdata2)

# t-test (equal variance)
t.test(LBXGH ~ depressed, data = nhanesdata2, var.equal=T)
t.test(log_A1c ~ depressed, data = nhanesdata2, var.equal=T)

# ANOVA (equivalent to t-test with equal variance)
a = aov(LBXGH ~ depressed, data = nhanesdata2)
anova(a)


## Example: Is A1c associated with bmi_cat?
# Here, we can only use ANOVA and not the t-test since bmi_cat is not binary

nhanesdata2 %>%
  group_by(bmi_cat) %>%
  summarise(mean_a1c = mean(LBXGH, na.rm=T),
            sd_a1c = sd(LBXGH, na.rm = T),
            n = n())
a2 = aov(LBXGH ~ bmi_cat, data = nhanesdata2)
anova(a2)

# We can opt to remove the 14.2-24.6 category since sample size is small
nhanesdata2 = nhanesdata2 %>%
  mutate(bmi_cat2 = factor(bmi_cat, levels = c('24.6-29.69', '29.69-33.5', '33.5-86.2' )))

nhanesdata2 %>%
  group_by(bmi_cat2) %>%
  summarise(mean_a1c = mean(LBXGH, na.rm=T),
            sd_a1c = sd(LBXGH, na.rm = T),
            n = n())
a3 = aov(LBXGH ~ bmi_cat2, data = nhanesdata2)
anova(a3)

# Exercise: Repeat the analysis using the log-transformed version of A1c.
