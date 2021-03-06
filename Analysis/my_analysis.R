# Packages ####
library(ggplot2)
library(plyr)
library(AICcmodavg)

# Data pre-processing ####

data_locations <- list.files(path="./Data", full.names=TRUE)

data_list <- lapply(data_locations, read.csv)

full_data <- Reduce(function(...){merge(..., all=T)}, data_list)

colnames(full_data) <- c("Lake",
                         "Gape",
                         "Plate",
                         "Specimen",
                         "Instar",
                         "Dorsal"
                         )
# Change instar to factor, not numeric
full_data$Instar <- as.factor(full_data$Instar)

# Exploration ####

# Kernel density of dorsal lengths
# Informal check of normality
dorsal_hist <- ggplot(full_data, aes(x=Dorsal)) + geom_density()
print(dorsal_hist)

# Kernel density of dorsal lengths
# Informal check of normality, 
# sample size and differences in mean / variability
dorsal_gape_hist <- ggplot(full_data, aes(x=Dorsal, colour=Gape)) + geom_density()
print(dorsal_gape_hist)

# Lake-by-lake differences
dorsal_lake_violin <- ggplot(full_data, aes(y=Dorsal, x=Lake)) + geom_violin() + facet_grid(facets=Gape~.)
print(dorsal_lake_violin)

# Instar differences
dorsal_instar_violin <- ggplot(full_data, aes(y=Dorsal, x=Instar)) + geom_violin() + facet_grid(facets=Gape~.)
print(dorsal_instar_violin)

# Lake-by-lake and instar differences
dorsal_lake_instar_violin <- ggplot(full_data, aes(y=Dorsal, x=Lake)) + geom_violin() + facet_grid(facets=Gape~Instar)
print(dorsal_lake_instar_violin)

# Regression  ####
# G=Gape, L=Lake, I=Instar

# Linear regression, null
lin_null <- lm(Dorsal~., data=full_data)

# Linear regression, G
lin_G <- lm(Dorsal~Gape, data=full_data)

# Linear regression, L
lin_L <- lm(Dorsal~Lake, data=full_data)

# Linear regression, I
lin_I <- lm(Dorsal~Instar, data=full_data)

# Linear regression, GL
lin_GL <- lm(Dorsal~Gape + Lake, data=full_data)

# Linear regression, GI
lin_GI <- lm(Dorsal~Gape + Instar, data=full_data)

# Linear regression, LI
lin_LI <- lm(Dorsal~Lake + Instar, data=full_data)

# Linear regression, GLI (full)
lin_GLI <- lm(Dorsal~Gape + Lake + Instar, data=full_data)

# GLM log-link regression, null
glm_null <- glm(Dorsal~., family=gaussian(link = "log"), data=full_data)

# GLM log-link regression, G
glm_G <- glm(Dorsal~Gape, family=gaussian(link = "log"), data=full_data)

# GLM log-link regression, L
glm_L <- glm(Dorsal~Lake, family=gaussian(link = "log"), data=full_data)

# GLM log-link regression, I
glm_I <- glm(Dorsal~Instar, family=gaussian(link = "log"), data=full_data)

# GLM log-link regression, GL
glm_GL <- glm(Dorsal~Gape + Lake, family=gaussian(link = "log"), data=full_data)

# GLM log-link regression, GI
glm_GI <- glm(Dorsal~Gape + Instar, family=gaussian(link = "log"), data=full_data)

# GLM log-link regression, LI
glm_LI <- glm(Dorsal~Lake + Instar, family=gaussian(link = "log"), data=full_data)

# GLM log-link regression, GLI (full)
glm_GLI <- glm(Dorsal~Gape + Lake + Instar, family=gaussian(link = "log"), data=full_data)

# Model comparison ####

# Linear models
lin_model_names <- c(
                     "lin_null",
                     "lin_G",
                     "lin_L",
                     "lin_I",
                     "lin_GL",
                     "lin_GI",
                     "lin_LI",
                     "lin_GLI"
                     )

lin_model_list <- list(
                    lin_null,
                    lin_G,
                    lin_L,
                    lin_I,
                    lin_GL,
                    lin_GI,
                    lin_LI,
                    lin_GLI
                  )

lin_AIC <- aictab(lin_model_list, lin_model_names, sort=FALSE)

# GLM log-link
glm_model_names <- c(
  "glm_null",
  "glm_G",
  "glm_L",
  "glm_I",
  "glm_GL",
  "glm_GI",
  "glm_LI",
  "glm_GLI"
)

glm_model_list <- list(
  glm_null,
  glm_G,
  glm_L,
  glm_I,
  glm_GL,
  glm_GI,
  glm_LI,
  glm_GLI
)

glm_AIC <- aictab(glm_model_list, glm_model_names, sort=FALSE)

# Model fit statistics

# Linear
lin_Rsq <- sapply(lin_model_list, function(x){summary(x)$r.squared})
lin_adj.Rsq <- sapply(lin_model_list, function(x){summary(x)$r.squared})
lin_sigma <- sapply(lin_model_list, function(x){summary(x)$sigma})
lin_fit <- data.frame(r.squared=lin_Rsq, adj.Rsq=lin_adj.Rsq, sigma=lin_sigma)
lin_fit <- cbind(lin_fit, lin_AIC)
rownames(lin_fit) <- lin_model_names
print(lin_fit)

# GLM
glm_dev.null <- sapply(glm_model_list, function(x){summary(x)$null.deviance})
glm_dev.resid <- sapply(glm_model_list, function(x){summary(x)$deviance})
glm_fit <- data.frame(dev.null=glm_dev.null, dev.resid=glm_dev.resid)
glm_fit <- cbind(glm_fit, glm_AIC)
rownames(glm_fit) <- glm_model_names
print(glm_fit)

# joint_fit <- cbind(joint_AIC, joint_RSq, joint_adj.RSq)

# Diagnostics and basic reporting ####
# Use best regression to test for differences
# Best regression is: GLM GLI
best_regression <- glm_GLI

# Extracting effects etc.
best_summary <- summary(best_regression)
best_coeff <- as.data.frame(best_summary$coefficients)

print(best_coeff)

# Adding confidence intervals
ci_level <- 0.975
best_coeff$upper <- qnorm(p=0.95, mean=best_coeff$Estimate, sd=best_coeff[["Std. Error"]])
best_coeff$lower <- qnorm(p=0.05, mean=best_coeff$Estimate, sd=best_coeff[["Std. Error"]])

# 

# Generating predicted values for each unique combination

# Propagating uncertainty


# Hypothesis testing ####

# Report effect sizes and p-values

# Effects and CI plots
# Need to show for every relevant combination, use predicted values with CIs


# Testing secondary prediction
# Dorsal fin length should increase by instar in gape-limited lakes
# But not others

#gape_limited_instar_effects <- data.frame(Instar=1:3, Effect=)

#not_gape_limited_instar_effects <- data.frame(Instar=1:3, Effect=)

# Trend test
gli_trend_test <- cor.test(x=gape_limited_instar_effects$Instar, y=gape_limited_instar_effects$Effect)

ngli_trend_test <- cor.test(x=not_gape_limited_instar_effects$Instar, y=not_gape_limited_instar_effects$Effect)

print(gli_trend_test)
print(ngli_trend_test)