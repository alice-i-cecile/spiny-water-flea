# Packages ####
library(ggplot2)
library(plyr)

# Data pre-processing ####

data_locations <- list.files(path="./Data", full.names=TRUE)

data_list <- lapply(list.files, read.csv)

full_data <- Reduce(function(...){merge(..., all=T)}, data_list)

colnames(full_data) <- c("Lake",
                         "Gape-limited",
                         "Plate",
                         "Specimen",
                         "Instar",
                         "Dorsal"
                         )

# Exploration ####

# Kernel density of dorsal lengths
# Informal check of normality
dorsal_hist <- ggplot(full_data, aes(x=Dorsal)) + geom_density()

# Kernel density of dorsal lengths
# Informal check of normality, 
# sample size and differences in mean / variability
dorsal_gape_hist <- ggplot(full_data, aes(x=Dorsal, colour=Gape-limited)) + geom_density()

# Lake-by-lake and instar differences
dorsal_lake_instar_violin <- ggplot(full_data, aes(y=Dorsal, x=Lake)) + geom_violin() + facet_grid(facets=Gape-limited~Instar)

# Instar differences
dorsal_instar_violin <- ggplot(full_data, aes(y=Dorsal, x=Instar)) + geom_violin() + facet_grid(facets=Gape-limited~)

# Regresssion ####



# Hypothesis testing ####

# Plots ####