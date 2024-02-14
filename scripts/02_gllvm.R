##
## Run GLLVMs
##
## ---------------------------------------------------------------------------- ##

##
## Run unconstrained GLLVMs, with and without predictors
##

## Libraries ####
library(gllvm)
library(dplyr)
library(ggplot2)

## Load data sets ####
load("./datasets/datasets.rda")

### Unconstrained ordinations based on null models ####
models_null <- list()

for (dataset in 1:length(datasets)) {
  
  name <- paste0(names(datasets[dataset]), "_null_model")
  data <- datasets[[dataset]]
  
  ## Species matrix
  Y_i <- data[, -1]
  
  ## Run model
  models_null[[name]] <-
    gllvm::gllvm(y = Y_i, 
                 num.lv = 2, 
                 family = "negative.binomial",
                 seed = 123)
  
  ## Residuals
  pdf(file = paste0("./results/residuals_", name,".pdf"))
  plot(models_null[[name]], which = 1:4, mfrow = c(2, 2))
  dev.off()
  
  rm("name", "data", "Y_i", "dataset")
}

## Ordination plots

for (model in 1:length(models_null)) {
  
  name <- names(models_null[model])
  data <- datasets[[model]]
  
  ## Get LV values and arrange it in a dataframe to plot
  data_to_plot <-
    cbind(season = data$season,
          as.data.frame(gllvm::getLV.gllvm(models_null[[model]])))
  
  ## Plot
  plot <- 
    ggplot(data = data_to_plot, 
           aes(x = LV1, y = LV2, color = season)) +
    geom_point() + 
    scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
    coord_cartesian(ylim = c(-3,3), xlim = c(-3,3)) +
    theme_bw() + 
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14))
  
  ggsave(plot,
         filename = paste0("./results/biplot_", name,".pdf"),
         height = 9, width = 12, units = "cm", dpi = 300)
  
  rm("name", "data", "data_to_plot", "plot", "model")
}

### Unconstrained ordinations based on models with predictors ####
models_predictor <- list()

for (dataset in 1:length(datasets)) {
  
  name <- paste0(names(datasets[dataset]), "_model_predictor")
  data <- datasets[[dataset]]
  
  ## Species matrix
  Y_i <- data[, -1]
  
  ## Run model
  models_predictor[[name]] <-
    gllvm::gllvm(y = Y_i, 
                 X = data.frame(season = data$season),
                 formula = ~season,
                 num.lv = 2, 
                 family = "negative.binomial",
                 seed = 123)
  
  ## Residuals
  pdf(file = paste0("./results/residuals_", name,".pdf"))
  plot(models_predictor[[name]], which = 1:4, mfrow = c(2, 2))
  dev.off()
  
  rm("name", "data", "Y_i", "dataset")
}

## Ordination plots

for (model in 1:length(models_predictor)) {
  
  name <- names(models_predictor[model])
  data <- datasets[[model]]
  
  ## Get LV values and arrange it in a dataframe to plot
  data_to_plot <-
    cbind(season = data$season,
          as.data.frame(gllvm::getLV.gllvm(models_predictor[[model]])))
  
  ## Plot
  plot <- 
    ggplot(data = data_to_plot, 
           aes(x = LV1, y = LV2, color = season)) +
    geom_point() + 
    scale_color_manual(values = c("summer" = "#E69F00", "winter" = "#56B4E9", "spring" = "grey50")) +
    # coord_cartesian(ylim = c(-3,3), xlim = c(-3,3)) +
    theme_bw() + 
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 14))
  
  ggsave(plot,
         filename = paste0("./results/biplot_", name,".pdf"),
         height = 9, width = 12, units = "cm", dpi = 300)
  
  rm("name", "data", "data_to_plot", "plot", "model")
}
