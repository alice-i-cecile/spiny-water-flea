# Libraries ####
library(ggplot2)
library(reshape)
library(car)

# Data sources ####


# Load the data ####

# Our group
waterflea <- read.csv("./Data/cecile-nelson-ocarroll.csv", na.strings="NA")

# Fix names
names(waterflea) <- c("Group",
                      "Folder",
                      "GLP",
                      "Picture",
                      "Individual",
                      "Instar",
                      "Sex",
                      "Spine",
                      "observation"
)

# Fix instar to be categorical
waterflea$Instar <- as.factor(waterflea$Instar)

# Standardize the data ####

# Using standard scores
waterflea$std <- (waterflea$Spine - mean(waterflea$Spine, na.rm=TRUE))/sd(waterflea$Spine, na.rm=TRUE)

# Calculate group means and std. dev ####
ggplot(waterflea[!is.na(waterflea$std),], aes(x=Instar, y=std)) + geom_violin(fill="black") + facet_grid(GLP~.) + theme_bw() + ylab("Standardized spine lengths") + geom_hline(y=0)

wf_mean <- cast(waterflea, GLP~Instar, fun.aggregate="mean", value="std", na.rm=TRUE)

wf_sd <- cast(waterflea, GLP~Instar, fun.aggregate="sd", value="std", na.rm=TRUE)

# Linear selection differential ####
std_glp <- waterflea[waterflea$GLP=="GLP", "std"]
std_nglp <- waterflea[waterflea$GLP=="NGLP", "std"]

lin_selection_diff <- mean(std_glp, na.rm=T) - mean(std_nglp, na.rm=T)

# T test
lin_t_test <- t.test(std_glp, std_nglp)


# Mann-Whitney U test
lin_MW_test <- wilcox.test(std_glp, std_nglp)

# Comparing results
print(lin_selection_diff)
print (lin_t_test)
print(lin_MW_test)

# Nonlinear selection differential ####
nonlin_selection_diff <- var(std_glp, na.rm=T) - var(std_nglp, na.rm=T) + lin_selection_diff^2

# Assuming normality
nonlin_F_test <- var.test(std_glp, std_nglp)

# Less strict normality (Brown–Forsythe test)
nonlin_BF_test <- leveneTest(y=waterflea$std, group=waterflea$GLP)

# Comparing results
print(nonlin_selection_diff)
print (nonlin_F_test)
print(nonlin_BF_test)

# Regression ####

# Linear regression, raw
wf_lm <- lm(Spine~GLP+Instar+GLP*Instar, waterflea)
print(summary(wf_lm))

# Linear regression, standardized
wf_lm_s <- lm(std~GLP+Instar+GLP*Instar, waterflea)
print(summary(wf_lm_s))

# GLM log regression, raw
wf_glm <- glm(Spine~GLP+Instar+GLP*Instar, waterflea, family=gaussian(link="log"))
print(summary(wf_glm))

# GLM log regression, standardized
wf_glm <- glm(std~GLP+Instar+GLP*Instar, waterflea, family=gaussian(link="log"))
print(summary(wf_glm))
