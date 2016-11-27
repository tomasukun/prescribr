#' Builder Class for Drug Class Study Groups
#' @import dplyr
#' @importFrom plyr mapvalues
#' @import stringr
#' @import tidyr
#' @import readr
#' @import purrr
#' @import lubridate
#' @importFrom Kmisc kLoad

build_drug_class <- R6::R6Class(
  'build_drug_class',
  inherit = study_pop_library,
  
  public = list(
    year = NULL,
    final_study_group = data_frame(), 
    partd_drug_class = data_frame(),
    open_pay_target = data_frame(),
    drug_class = NULL,
    target_drug = NULL,
    
    initialize = function(yr = '2014', class = 'statins', target_drug = 'CRESTOR') {
      self$year <- yr
      self$drug_class <- class
      self$target_drug <- target_drug
    },
    
    read_partd_drug_class = function() {
      
      self$partd_drug_class <- Kmisc::kLoad(paste0(self$processed_file_dir, self$year, 
                                            '/partd_combined.rData')) %>% 
        select(NPI, doc_drug_brand_name, doc_drug_total_claims, doc_drug_total_day_supply, 
               doc_drug_total_drug_cost, doc_drug_bene_count) %>% 
        filter(
          str_detect(doc_drug_brand_name, self$study_drugs[[self$drug_class]]),
          !is.na(doc_drug_total_claims),
          doc_drug_total_claims >= self$exclusion_criteria$claim_count
        ) %>% 
        group_by(NPI) %>% 
        mutate(
          total_class_bene = sum(doc_drug_bene_count),
          total_target_bene = sum(doc_drug_bene_count[doc_drug_brand_name == self$target_drug],
                                  na.rm = TRUE),
          total_class_claims = sum(doc_drug_total_claims),
          total_target_claims = sum(doc_drug_total_claims[doc_drug_brand_name == self$target_drug],
                                    na.rm = TRUE),
          total_class_day_supply = sum(doc_drug_total_day_supply),
          total_target_day_supply = sum(doc_drug_total_day_supply[doc_drug_brand_name == self$target_drug],
                                        na.rm = TRUE),
          total_class_drug_cost = sum(doc_drug_total_drug_cost),
          total_target_drug_cost = sum(doc_drug_total_drug_cost[doc_drug_brand_name == self$target_drug],
                                       na.rm = TRUE)
        ) %>% 
        select(-doc_drug_bene_count, -doc_drug_total_claims, -doc_drug_brand_name,
               -doc_drug_total_day_supply, -doc_drug_total_drug_cost) %>% 
        distinct(NPI, .keep_all = TRUE) %>% 
        ungroup()
      
    },
    
    merge_partd_drug_class = function() {
      
      self$drug_class <- Kmisc::kLoad(paste0(self$processed_file_dir, self$year,
                                             '/final_study_population.rData'))
      self$drug_class <- self$drug_class %>% 
        inner_join(self$partd_drug_class, by = 'NPI')
        
    },
    
    read_openpay_drug_class = function() {
      
      self$open_pay_target <- Kmisc::kLoad(paste0(self$processed_file_dir, self$year,
                                     '/open_payments.rData'))
        
                   
                              
    },
    
    merge_openpay_drug_class = function() {
      
    },
    
    build_tables = function() {
      self$read_partd_drug_class()
      self$merge_partd_drug_class()
      self$read_openpay_drug_class()
      self$merge_openpay_drug_class()
    }
    
  )
)
    