#' Analysis class
#' @import dplyr
#' @importFrom plyr mapvalues
#' @import stringr
#' @import tidyr

build_analysis <- R6::R6Class(
  'build_analysis',
  inherit = build_changer_data,
  public = list(
    drug_class = '',
    type = '',
    analysis_data = data_frame(),
    
    initialize = function(data, class, type = NULL) {
      self$analysis_data <- data
      self$type <- type
      self$drug_class <- class
    },
    
    build_analysis_vars = function() {
      self$analysis_data <- self$analysis_data %>% 
        mutate(spec_cat = if_else(doc_specialty %in% c('Internal Medicine', 'Hospitalist'),
                                  "Internal Medicine",
                                  if_else(doc_specialty %in% c('Family Practice', 'Family Medicine', 'General Practice'),
                                          'Family Medicine', "Other")),
               grad_year_cat = if_else((2013 - doc_grad_year) <= 5, 1,
                                       if_else((2013 - doc_grad_year) > 20, 3, 2)),
               total_vol_per100 = doc_total_claims/100,
               paid = if_else(payment_count > 0, 1, 0),
               year = if_else(year == '2013', 0, 1),
               did = paid*year
        )
    },
    
    build_model = function() {
      if(self$type == 'did') {
        cat('Difference-in-Difference Analysis (DID) \n')
        did_glm <- glm(cbind(target_claims, class_claims) ~ factor(paid) + factor(year) + did + 
                         factor(spec_cat) + factor(grad_year_cat) + total_vol_per100,
                       data = self$analysis_data, family = 'binomial')
        summary(did_glm)
        cat('Odds Ratios \n')
        exp(cbind(OR = coef(did_glm), confint(did_glm)))
        
        cat('DID with Robust Standard Errors \n')
        cov <- sandwich::vcovHC(did_glm, type = "HC0")
        std_err <- sqrt(diag(cov))
        estimation <- data_frame(
          coefficients = names(did_glm$coefficients),
          estimate = coef(did_glm), 
          "RSE" = std_err,
          z = (coef(did_glm)/std_err),
          "P" = 2 * pnorm(abs(coef(did_glm)/std_err), lower.tail = FALSE),
          LL = coef(did_glm) - qnorm(0.975)  * std_err,
          UL = coef(did_glm) + qnorm(0.975)  * std_err
        )
        estimation
        cat('Odds Ratios \n')
        or_estimation <- estimation %>% 
          select(coefficients, estimate, LL, UL) %>% 
          mutate_if(is.numeric, exp)
        or_estimation
      } else {
        cat(sprintf('%s is not a supported analysis method \n\n analysis data not created', self$type))
      }
    }
  )
)