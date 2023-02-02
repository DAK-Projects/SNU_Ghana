library(readxl)
library(stats)
library(lmtest)
library(tidyverse)
library(rstatix)
library(ggpubr)
gh_snu_2 <- read_excel("gh_snu_2.xlsx")
overall <- read_excel("gh_snu_2.xlsx",sheet="overall")

#table 1 anova
model <- aov( overall_mean ~ Performance, data = overall)
summary(model)

#table 2
capacities <-read_excel("cap.xlsx", sheet="sp")
capacities <-read_excel("cap.xlsx")
res.aov <- aov(Mean ~ Capacity, data = capacities)
summary(res.aov)
pairwise.t.test(capacities$Mean, capacities$Capacity, p.adjust.method="bonferroni")

#table 3-service provision
sp <-read_excel("cap2.xlsx",sheet="sp")
res.aov <- aov(Mean_value ~ Dimension, data = sp)
summary(res.aov)
pairwise.t.test(sp$Mean_value, sp$Dimension, p.adjust.method="bonferroni")

#table 3-management capacity
mc <-read_excel("cap2.xlsx",sheet="mc")
res.aov <- aov(Mean_value ~ Dimensions, data = mc)
summary(res.aov)
pairwise.t.test(mc$Mean_value, mc$Dimensions, p.adjust.method="bonferroni")

#table 3 oversight capacity
oc <-read_excel("cap2.xlsx",sheet="oc")
res.aov <- aov(Mean_value ~ Dimension, data = oc)
summary(res.aov)
pairwise.t.test(oc$Mean_value, oc$Dimension, p.adjust.method="bonferroni")