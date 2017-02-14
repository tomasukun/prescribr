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
                                          'Family Medicine', 
                                          if_else(!(doc_specialty %in% c("Family Practice", "Family Medicine", "General Practice",
                                                                         "Internal Medicine", "Hospitalist"))))),
               grad_year_cat = if_else((2013 - doc_grad_year) <= 5, 1,
                                       if_else((2013 - doc_grad_year) > 20, 3, 2)),
               total_vol_per100 = doc_total_claims/100,
               paid = if_else(payment_count > 0, 1, 0),
               year = if_else(year == '2013', 0, 1),
               did = paid*year
        )
    },
    
    build_analysis = function() {
      if(self$type == 'did') {
        did_glm <- glm(cbind(target_claims, class_claims) ~ factor(paid) + factor(year) + did + 
                         factor(spec_cat) + factor(grad_year_cat) + total_vol_per100,
                       data = self$analysis_data, family = 'binomial')
        summary(did_glm)
      } else {
        cat(sprintf('%s is not a supported analysis method \n\n analysis data not created', self$type))
      }
    }
    
    
  )
)