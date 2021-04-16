ep_model <- NULL
fg_model <- NULL
wp_model <- NULL
load("models/sysdata.rda")

# View the object sizes for each (so you know why we deleted what we did):
sapply(ep_model, object.size)
sapply(fg_model, object.size)
sapply(wp_model, object.size)

# Remove unused elements of EP model:
ep_model$residuals <- NULL
ep_model$fitted.values <- NULL
ep_model$weights <- NULL
save(ep_model, file = 'models/ep_model.Rdata')

# Remove unused elements of FG:
fg_model$offset <- NULL
fg_model$residuals <- NULL
fg_model$y <- NULL
fg_model$na.action <- NULL
fg_model$fitted.values <- NULL 
fg_model$working.weights <- NULL
fg_model$prior.weights <- NULL
fg_model$linear.predictors <- NULL
fg_model$weights <- NULL
fg_model$dw.drho <- NULL
fg_model$hat <- NULL
fg_model$deviance <- NULL
fg_model$null.deviance <- NULL
fg_model$iter <- NULL
fg_model$df.null <- NULL
fg_model$converged <- NULL
fg_model$boundary <- NULL
fg_model$rV <- NULL
fg_model$db.drho <- NULL
fg_model$control <- NULL
fg_model$R <- NULL
fg_model$Ve <- NULL
save(fg_model, file = 'models/fg_model.Rdata')

# Remove unused elements of WP model:
wp_model$offset <- NULL 
wp_model$residuals <- NULL
wp_model$y <- NULL  
wp_model$na.action <- NULL 
wp_model$fitted.values <- NULL 
wp_model$working.weights <- NULL
wp_model$prior.weights <- NULL
wp_model$linear.predictors <- NULL
wp_model$weights <- NULL
wp_model$dw.drho <- NULL
wp_model$hat <- NULL
wp_model$deviance <- NULL
wp_model$null.deviance <- NULL
wp_model$iter <- NULL
wp_model$df.null <- NULL
wp_model$converged <- NULL
wp_model$boundary <- NULL
wp_model$rV <- NULL
wp_model$db.drho <- NULL
wp_model$control <- NULL
wp_model$R <- NULL
wp_model$Ve <- NULL
save(wp_model, file = 'models/wp_model.Rdata')
# # The following commented code is how the models are then included in cfbscrapR:
# library(devtools)
# use_data(fg_model, ep_model, wp_model,
#          internal = TRUE, compress = "xz", overwrite = TRUE)
