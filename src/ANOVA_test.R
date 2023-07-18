library(datasets)
library(ggplot2)
library(multcompView)
library(dplyr)

anova_gs <- aov(grain.set ~ time_id, data = grain_set)
summary(anova_gs)

# Tukey's test
tukey_gs <- TukeyHSD(anova_gs)
print(tukey_gs)

# compact letter display
cld_gs <- multcompLetters4(anova_gs, tukey_gs)
print(cld_gs)

# table with factors and 3rd quantile
Tk_gs <- group_by(grain_set, time_id) %>%
  summarise(mean=mean(grain.set), quant = quantile(grain.set, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$feed)
Tk$cld <- cld$Letters

print(Tk)

##Flower batch comparison
anova_gs <- aov(grain.set ~ time_id, data = grain_set)
summary(anova_gs)

# Tukey's test
tukey_gs <- TukeyHSD(anova_gs)
print(tukey_gs)

# compact letter display
cld_gs <- multcompLetters4(anova_gs, tukey_gs)
print(cld_gs)

# table with factors and 3rd quantile
Tk_gs <- group_by(grain_set, time_id) %>%
  summarise(mean=mean(grain.set), quant = quantile(grain.set, probs = 0.75)) %>%
  arrange(desc(mean))

# extracting the compact letter display and adding to the Tk table
cld <- as.data.frame.list(cld$feed)
Tk$cld <- cld$Letters

print(Tk)
