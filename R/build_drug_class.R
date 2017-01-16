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
    partd_target_drug = NULL,
    openpay_target_drug = NULL,
    target_manufacturer = NULL,
    
    initialize = function(yr = '2014', class = 'statins', target_drug = 'crestor') {
      self$year <- yr
      self$drug_class <- class
      self$partd_target_drug <- plyr::mapvalues(target_drug,
                                           names(self$partd_target),
                                           self$partd_target)
      self$openpay_target_drug <- plyr::mapvalues(target_drug,
                                          names(self$open_payments_target),
                                          self$open_payments_target)
      self$target_manufacturer <- plyr::mapvalues(target_drug,
                                                  names(self$target_drug_manufacturer),
                                                  self$target_drug_manufacturer)
    },
    
    read_partd_drug_class = function() {
      
      self$partd_drug_class <- Kmisc::kLoad(paste0(self$processed_file_dir, self$year, 
                                            '/partd_combined.rData')) %>% 
        select(NPI, 
               doc_drug_class_brand_name = doc_drug_brand_name, 
               doc_drug_class_claims = doc_drug_total_claims,
               doc_drug_class_day_supply = doc_drug_total_day_supply, 
               doc_drug_class_cost = doc_drug_total_drug_cost,
               doc_drug_class_bene_count = doc_drug_bene_count) %>% 
        filter(
          str_detect(doc_drug_class_brand_name, self$study_drugs[[self$drug_class]])
        )
        
      
    },
    
    merge_partd_drug_class = function() {
      
      self$final_study_group <- Kmisc::kLoad(paste0(self$processed_file_dir, self$year,
                                             '/final_study_population.rData'))
      self$final_study_group <- self$final_study_group %>% 
        inner_join(self$partd_drug_class, by = 'NPI') %>% 
        filter(
          !is.na(doc_drug_class_claims),
          doc_drug_class_claims >= self$exclusion_criteria$claim_count
        ) %>% 
        group_by(NPI) %>% 
        mutate(
          total_class_bene = sum(doc_drug_class_bene_count),
          total_target_bene = sum(doc_drug_class_bene_count[doc_drug_class_brand_name == self$partd_target_drug],
                                  na.rm = TRUE),
          total_class_claims = sum(doc_drug_class_claims),
          total_target_claims = sum(doc_drug_class_claims[doc_drug_class_brand_name == self$partd_target_drug],
                                    na.rm = TRUE),
          total_class_day_supply = sum(doc_drug_class_day_supply),
          total_target_day_supply = sum(doc_drug_class_day_supply[doc_drug_class_brand_name == self$partd_target_drug],
                                        na.rm = TRUE),
          total_class_drug_cost = sum(doc_drug_class_cost),
          total_target_drug_cost = sum(doc_drug_class_cost[doc_drug_class_brand_name == self$partd_target_drug],
                                       na.rm = TRUE)
        ) %>% 
        select(-doc_drug_class_bene_count, -doc_drug_class_claims, -doc_drug_class_brand_name,
               -doc_drug_class_day_supply, -doc_drug_class_cost) %>% 
        distinct(NPI, .keep_all = TRUE) %>% 
        ungroup()
      # number of docs with over 100 claims in class
      self$study_group_pop[[paste0(self$drug_class, '_', self$year)]]$claim_count_100plus <- nrow(self$final_study_group)
        
    },
    
    read_openpay_drug_class = function() {
      
      self$open_pay_target <- Kmisc::kLoad(paste0(self$processed_file_dir, self$year,
                                     '/open_payments.rData')) %>% 
        select(doc_id, drug_manufacturer, payment_dollars, payment_number, payment_type, 
               starts_with('payment_drug'), starts_with('payment_device'))
      
    },
    
    merge_openpay_drug_class = function() {

        # distinct NOT PAID docs
        study_group_not_paid <- self$final_study_group %>% 
          filter(is.na(doc_id))
        # PAID docs
        study_group_paid <- self$final_study_group %>% 
          filter(!is.na(doc_id)) %>% 
          inner_join(self$open_pay_target, by = 'doc_id') %>% 
          filter(
            !(stringr::str_detect(drug_manufacturer, self$target_manufacturer) &
              is.na(payment_drug_1) &
              is.na(payment_drug_2) &
              is.na(payment_drug_3) &
              is.na(payment_drug_4) &
              is.na(payment_drug_5) &
              is.na(payment_device_1) &
              is.na(payment_device_2) &
              is.na(payment_device_3) &
              is.na(payment_device_4) &
              is.na(payment_device_5))
            )
          
        # number of docs who received a tagged payment
        self$study_group_pop[[paste0(self$drug_class, '_', self$year)]]$tagged_payment <- nrow(distinct(study_group_paid, NPI))
        study_group_paid <- study_group_paid %>%  
          filter(payment_type == 'Food and Beverage')
        # number of docs who recieved a food and beverage payment
        self$study_group_pop[[paste0(self$drug_class, '_', self$year)]]$meal_payment <- nrow(distinct(study_group_paid, NPI))
        
        # distinct TARGET PAID docs
        target_drug_regex <- str_c(self$openpay_target_drug, tolower(self$openpay_target_drug),
                                    str_replace(tolower(self$openpay_target_drug), str_sub(tolower(self$openpay_target_drug), 1, 1), 
                                                toupper(str_sub(self$openpay_target_drug, 1, 1))), 
                                    sep = '|')
        study_group_target_paid <- study_group_paid %>% 
          filter(
            (str_detect(payment_drug_1, target_drug_regex) |
               str_detect(payment_drug_2, target_drug_regex) |
               str_detect(payment_drug_3, target_drug_regex) |
               str_detect(payment_drug_4, target_drug_regex) |
               str_detect(payment_drug_5, target_drug_regex))
          ) %>% 
          group_by(NPI) %>% 
          mutate(
            total_target_payment_dollars = sum(payment_dollars, na.rm = TRUE),
            total_target_payment_number  = sum(payment_number, na.rm = TRUE)
          ) %>% 
          select(-drug_manufacturer, -payment_dollars, -payment_number, -payment_type, 
                 -starts_with('payment_drug'), -starts_with('payment_device')) %>% 
          distinct(NPI, .keep_all = TRUE) %>% 
          ungroup()
        # distinct NON-TARGET PAID docs
        study_group_paid_non_target <- study_group_paid %>% 
          anti_join(study_group_target_paid, by = 'doc_id') %>% 
          select(-drug_manufacturer, -payment_dollars, -payment_number, -payment_type, 
                 -starts_with('payment_drug'), -starts_with('payment_device')) %>% 
          distinct(NPI, .keep_all = TRUE)
        
        self$final_study_group <- bind_rows(
          study_group_not_paid, 
          study_group_paid_non_target,
          study_group_target_paid
        )
    },
    
    save_drug_class = function() {
      study_group_census <- self$study_group_pop
      save(study_group_census, file = 'data/study_group_pop.rData')
      
      final_study_group <- self$final_study_group
      save(final_study_group,
           file = paste0('data/source_tables/', 'study_group_', self$drug_class, '_', self$year, '.rData'))
    },
    
    build_tables = function() {
      self$read_partd_drug_class()
      self$merge_partd_drug_class()
      self$read_openpay_drug_class()
      self$merge_openpay_drug_class()
      self$save_drug_class()
    }
    
  )
)
    