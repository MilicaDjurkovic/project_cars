
# Load the required libraries
library(tidyverse)
library(dplyr)
library(caret)
library(rpart)
library(corrplot)
library(car)
library(ggplot2)
library(ggrepel)
library(ggfortify)

# Load and preprocess data
load_and_preprocess_data <- function(data_dir) {
    data_dir <- paste0(getwd(), "/_data/")
    cars_data <- read.csv(paste0(data_dir, "cars.csv"),
                          header = TRUE,
                          check.names = FALSE,
                          stringsAsFactors = FALSE)
    
    cars_data$speed_kmh <- cars_data$speed * 1.60934
    cars_data$dist_m <- cars_data$dist * 0.3048
    
    cars_data <- cars_data  %>%
        select("speed_kmh", "dist_m")
    
    return(cars_data)
}

cars_data <- load_and_preprocess_data(data_dir)

# Fit Linear Model
# -- Simple Linear Regression
# - Target: predict dist_m from speed_kmh
# - cars_data

fit_linear_model <- function(cars_data) {
    
    linear_model <- lm(dist_m ~ speed_kmh, data = cars_data)
    return(linear_model)
}

linear_model <- fit_linear_model(cars_data)
print(summary(linear_model))

# Showing basic statistics for numeric variables
summary(cars_data)

# Correlation between distance and speed
corr <- cor(cars_data$speed_kmh, cars_data$dist_m) 


# Plot Linear Model
plot_linear_model <- function(model, cars_data){
    
    cars_data$predicted <- model$fitted.values
    cars_data$residuals <- model$residuals
    
    # - visualize relationship
    ggplot(data = cars_data,
           aes(x = speed_kmh, y = dist_m)) +
        geom_smooth(method = lm, se = FALSE, color = "red", size = .25) +
        geom_segment(aes(x = speed_kmh, 
                         y = predicted, 
                         xend = speed_kmh, 
                         yend = predicted + residuals),
                     color = "black", size = .2, linetype = "dashed") +
        geom_point(aes(x = speed_kmh, y = dist_m), color = "black", size = 1) +
        geom_point(aes(x = speed_kmh, y = dist_m), color = "white", size = .5) +
        geom_point(aes(x = speed_kmh, y = predicted), color = "red", size = 1) +
        ggtitle("dist_m ~ speed_kmh") +
        theme_bw() + 
        theme(panel.border = element_blank()) +
        theme(plot.title = element_text(hjust = .5))
}

plot_linear_model(linear_model, cars_data)

calculate_correlation_r2 <- function(data) {
    r <- cor.test(data$speed_kmh, data$dist_m)
    r_estimate <- r$estimate
    r_squared <- r_estimate^2
    return(list(r, r_estimate, r_squared))
}

correlation_r2 <- calculate_correlation_r2(cars_data)

# Inspect linear model
inspect_linear_model <- function(model) {
    coef_summary <- summary(model)$coefficients
    conf_interval <- confint(model, level = 0.99)
    residuals <- model$residuals
    sse <- sum(residuals^2)
    fitted_values <- model$fitted.values
    return(list(coef_summary, conf_interval, residuals, sse, fitted_values))
}

linear_model_inspection <- inspect_linear_model(linear_model)

### --- Predicting new data
new_cars_speed = rnorm(100, mean = mean(cars_data$speed_kmh), 
                        sd = sd(cars_data$speed_kmh))

# Predict new data
predict_new_data <- function(model, new_speed_data) {
    predictions <- data.frame(speed_kmh = new_speed_data, 
                              dist_m = predict(model, newdata = data.frame(speed_kmh = new_speed_data)))
    return(predictions)
}

predictions <- predict_new_data(linear_model, new_cars_speed)

# Plot model predictions
plot_model_predictions <- function(predictions) {
    ggplot(data = predictions, aes(x = speed_kmh, y = dist_m)) + 
        geom_smooth(method = lm, se = FALSE, size = 0.25, color = "red") +
        geom_point(color = "black", size = 2) + 
        geom_point(color = "white", size = 1.5) +
        ggtitle("Predictions") + 
        theme_bw() +
        theme(panel.border = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5))
}

plot_model_predictions(predictions)

### --- The distribution of residuals


# Plot distribution of residuals
plot_residual_distribution <- function(model) {
    model_residuals <- data.frame(residuals = model$residuals)
    ggplot(data = model_residuals, aes(x = residuals)) + 
        geom_histogram(binwidth = 10) + 
        ggtitle("Model Residuals") +
        theme_bw() + 
        theme(panel.border = element_blank()) + 
        theme(plot.title = element_text(hjust = 0.5))
}

plot_residual_distribution(linear_model)



# Plot QQ Plot of Model Residuals
plot_qqplot <- function(model) {
    model_residuals <- data.frame(residuals = model$residuals)
    ggplot(model_residuals, aes(sample = residuals)) + 
        stat_qq() + 
        stat_qq_line() + 
        ggtitle("Q-Q Plot of Model Residuals") +
        xlab("Theoretical Quantiles") + ylab("Sample Quantiles") + 
        theme_bw() + 
        theme(panel.border = element_blank()) + 
        theme(plot.title = element_text(hjust = 0.5))
}
plot_qqplot(linear_model)

# Plot Predicted vs. Residuals
plot_predicted_vs_residuals <- function(model) {
    predictions <- model$fitted.values
    residuals <- model$residuals
    predicted_res_frame <- data.frame(predicted = predictions, residual = residuals)
    
    ggplot(data = predicted_res_frame, aes(x = predicted, y = residual)) +
        geom_point(size = 1.5) +
        geom_smooth(method = 'lm', size = 0.25, alpha = 0.25, color = "red", se = FALSE) + 
        ggtitle("Predicted vs Residual") + 
        xlab("Predicted") + ylab("Residual") + 
        theme_bw() +
        theme(legend.position = "none") + 
        theme(panel.border = element_blank()) + 
        theme(plot.title = element_text(hjust = 0.5))
}

plot_predicted_vs_residuals(linear_model)

# Identify influential cases using Cook's distance
identify_influential_cases <- function(model, threshold = 1) {
    inf_frame <- influence.measures(model)
    inf_frame <- as.data.frame(inf_frame$infmat)
    
    w_cookD <- which(inf_frame$cook.d > threshold)
    return(w_cookD)
}

w_cookD <- identify_influential_cases(linear_model, threshold = 1)
print(w_cookD)

inf_frame <- influence.measures(linear_model)
inf_frame <- as.data.frame(inf_frame$infmat)

# Identify cases with high leverage
identify_high_leverage_cases <- function(inf_frame, k = 1, n) {
    w_leverage <- which(inf_frame$hat > 2 * ((k + 1) / n))
    return(w_leverage)
}
n <- dim(cars_data)[1]
w_leverage <- identify_high_leverage_cases(inf_frame, k = 1, n = n)
print(w_leverage)

# Plot influence plot
plot_influence_plot <- function(model, inf_frame) {
    inf_plot_frame <- data.frame(
        residual = model$residuals,
        leverage = inf_frame$hat,
        cookD = inf_frame$cook.d
    )
    
    ggplot(inf_plot_frame,
           aes(y = residual, x = leverage)) +
        geom_point(size = inf_plot_frame$cookD * 100, 
                   shape = 1, 
                   color = "blue") +
        ggtitle("Influence Plot\nSize of the blob corresponds to Cook's distance") +
        theme(plot.title = element_text(size = 8, face = "bold")) +
        ylab("Standardized Residual") + xlab("Leverage") + 
        theme_bw() + 
        theme(panel.border = element_blank()) + 
        theme(plot.title = element_text(hjust = 0.5))
}

plot_influence_plot(linear_model, inf_frame)


function_names <- c("load_and_preprocess_data",
                    "fit_linear_model",
                    "plot_linear_model",
                    "calculate_correlation_r2",
                    "inspect_linear_model",
                    "predict_new_data",
                    "plot_model_predictions",
                    "plot_residual_distribution" ,
                    "plot_qqplot", 
                    "plot_predicted_vs_residuals", 
                    "identify_influential_cases", 
                    "identify_high_leverage_cases", 
                    "plot_influence_plot")
                    
                    
dump(function_names, file = "00_scripts_data_and_plots.R")
                    
                    



