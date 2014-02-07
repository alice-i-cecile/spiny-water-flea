# Libraries ####

library(ggplot2)
library(reshape)


# Load the data ####

# Our group
waterflea <- read.csv("./Data/cecile-nelson-ocaroll.csv", na.strings="NA")

# Class data
#waterflea <- read.csv("./Data/???.csv", na.value="NA")

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
ggplot(waterflea[!is.na(waterflea$std),], aes(x=Instar, y=std)) + geom_violin(fill="black") + facet_grid(GLP~.) + theme_bw()

wf_mean <- cast(waterflea, GLP~Instar, fun.aggregate="mean", value="std", na.rm=TRUE)

wf_sd <- cast(waterflea, GLP~Instar, fun.aggregate="sd", value="std", na.rm=TRUE)

# Test for significance ####

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
