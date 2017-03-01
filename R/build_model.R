#' Analysis class
#' @import dplyr
#' @importFrom plyr mapvalues
#' @import stringr
#' @import tidyr

build_model <- R6::R6Class(
  'build_model',
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
    
    build_model_vars = function() {
      model_spec = self$analysis_vars
      
      self$analysis_data <- self$analysis_data %>% 
        mutate(grad_year_cat = if_else((2013 - doc_grad_year) <= 5, 1,
                                       if_else((2013 - doc_grad_year) > 20, 3, 2)),
               total_vol_per100 = doc_total_claims/100,
               paid = if_else(payment_count > 0, 1, 0),
               year = if_else(year == '2013', 0, 1),
               did = paid*year,
               region = if_else(doc_state %in% model_spec$region$`Northeast`, 'Northeast',
                                   if_else(doc_state %in% model_spec$region$`Midwest`, 'Midwest', 
                                           if_else(doc_state %in% model_spec$region$`South`, 'South',
                                                   if_else(doc_state %in% model_spec$region$`Pacific West`, 'Pacific West',
                                                           "Mountain West")))),
               group_size = if_else(doc_group_size < 2, 1,
                                    if_else(doc_group_size >= 2 & doc_group_size < 11, 2,
                                            if_else(doc_group_size >= 11 & doc_group_size < 51, 3, 4))),
               gender = if_else(doc_gender == 'M', 0, 1),
               doc_lis = round((doc_lis_claims/doc_total_claims)*100, 1),
               doc_mapd = round((doc_mapd_claims/doc_total_claims)*100, 1)
        )
      if(self$drug_class %in% c('opthalmic_corticosteroid', 'opthalmic_antibiotic')) {
        self$analysis_data <- self$analysis_data %>% 
          mutate(
            spec_cat = if_else(!(doc_specialty %in% model_spec$doc_spec$`Ophthalmology`), "Other", 'Ophthalmology'))
      } else if(self$drug_class %in% 'vaginal_cream') {
        self$analysis_data <- self$analysis_data %>% 
          mutate(
            spec_cat = if_else(doc_specialty %in% model_spec$doc_spec$`OBGYN`, 'OBGYN', "Other"))
      } else if(self$drug_class %in% 'opioids') {
        self$analysis_data <- self$analysis_data %>% 
          mutate(
            spec_cat = if_else(doc_specialty %in% model_spec$doc_spec$`Family Medicine`, 'Family Medicine',
                               if_else(doc_specialty %in% model_spec$doc_spec$`Pain Management`, 'Pain Management',
                                       if_else(doc_specialty %in% model_spec$doc_spec$`Internal Medicine`, "Internal Medicine", "Other"))))
      } else {
        self$analysis_data <- self$analysis_data %>% 
          mutate(
            spec_cat = if_else(doc_specialty %in% model_spec$doc_spec$`Internal Medicine`, "Internal Medicine", 
                               if_else(doc_specialty %in% model_spec$doc_spec$`Family Medicine`, 
                                       'Family Medicine', "Other")))
      }
    },
    
    build_model_estimates = function(class = self$drug_class) {
      if(self$type == 'did') {
        message('Difference-in-Difference Analysis (DID) \n')
        if(class %in% c('opthalmic_corticosteroid', 'opthalmic_antibiotic')) {
          did_glm <- glm(cbind(target_claims, class_claims) ~ factor(paid) + factor(year) + did + 
                           factor(grad_year_cat) + total_vol_per100 +
                           factor(region) + factor(group_size) + 
                           factor(gender) + doc_lis + doc_mapd,
                         data = self$analysis_data, family = 'binomial')
        } else {
          did_glm <- glm(cbind(target_claims, class_claims) ~ factor(paid) + factor(year) + did + 
                           factor(spec_cat) + factor(grad_year_cat) + total_vol_per100 +
                           factor(region) + factor(group_size) + 
                           factor(gender) + doc_lis + doc_mapd,
                         data = self$analysis_data, family = 'binomial')
        }

        summary(did_glm)
        message('Odds Ratios \n')
        exp(cbind(OR = coef(did_glm), confint(did_glm)))
        
        message('DID with Robust Standard Errors \n')
        cov <- sandwich::vcovHC(did_glm, type = "HC0")
        std_err <- sqrt(diag(cov))
        estimation <- data_frame(coefficients = names(did_glm$coefficients), 
                                 estimate = coef(did_glm),
                                 "RSE" = std_err, 
                                 z = (coef(did_glm)/std_err), 
                                 "P" = 2 * pnorm(abs(coef(did_glm)/std_err), lower.tail = FALSE), 
                                 LL = coef(did_glm) - qnorm(0.975)  * std_err, 
                                 UL = coef(did_glm) + qnorm(0.975)  * std_err)
        estimation
        message('Odds Ratios \n')
        or_estimation <- estimation %>% 
          select(coefficients, estimate, LL, UL) %>% 
          mutate_if(is.numeric, exp)
        or_estimation
      } else {
        cat(sprintf('%s is not a supported method \n\n analysis not run', self$type))
      }
    }
  )
)