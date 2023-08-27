load_and_preprocess_data <-
function(data_dir) {
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
fit_linear_model <-
function(cars_data) {
    
    linear_model <- lm(dist_m ~ speed_kmh, data = cars_data)
    return(linear_model)
}
plot_linear_model <-
function(model, cars_data){
    
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
calculate_correlation_r2 <-
function(data) {
    r <- cor.test(data$speed_kmh, data$dist_m)
    r_estimate <- r$estimate
    r_squared <- r_estimate^2
    return(list(r, r_estimate, r_squared))
}
inspect_linear_model <-
function(model) {
    coef_summary <- summary(model)$coefficients
    conf_interval <- confint(model, level = 0.99)
    residuals <- model$residuals
    sse <- sum(residuals^2)
    fitted_values <- model$fitted.values
    return(list(coef_summary, conf_interval, residuals, sse, fitted_values))
}
predict_new_data <-
function(model, new_speed_data) {
    predictions <- data.frame(speed_kmh = new_speed_data, 
                              dist_m = predict(model, newdata = data.frame(speed_kmh = new_speed_data)))
    return(predictions)
}
plot_model_predictions <-
function(predictions) {
    ggplot(data = predictions, aes(x = speed_kmh, y = dist_m)) + 
        geom_smooth(method = lm, se = FALSE, size = 0.25, color = "red") +
        geom_point(color = "black", size = 2) + 
        geom_point(color = "white", size = 1.5) +
        ggtitle("Predictions") + 
        theme_bw() +
        theme(panel.border = element_blank()) +
        theme(plot.title = element_text(hjust = 0.5))
}
plot_residual_distribution <-
function(model) {
    model_residuals <- data.frame(residuals = model$residuals)
    ggplot(data = model_residuals, aes(x = residuals)) + 
        geom_histogram(binwidth = 10) + 
        ggtitle("Model Residuals") +
        theme_bw() + 
        theme(panel.border = element_blank()) + 
        theme(plot.title = element_text(hjust = 0.5))
}
plot_qqplot <-
function(model) {
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
plot_predicted_vs_residuals <-
function(model) {
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
identify_influential_cases <-
function(model, threshold = 1) {
    inf_frame <- influence.measures(model)
    inf_frame <- as.data.frame(inf_frame$infmat)
    
    w_cookD <- which(inf_frame$cook.d > threshold)
    return(w_cookD)
}
identify_high_leverage_cases <-
function(inf_frame, k = 1, n) {
    w_leverage <- which(inf_frame$hat > 2 * ((k + 1) / n))
    return(w_leverage)
}
plot_influence_plot <-
function(model, inf_frame) {
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
