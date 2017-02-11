#' Builder for changer analysis, comparing two years of prescribing and payments
#' @import dplyr
#' @importFrom plyr mapvalues
#' @import stringr
#' @import tidyr
#' @import readr
#' @import purrr
#' @import ggplot2
#' @importFrom Kmisc kLoad
 
build_changer_data <- R6::R6Class(
  'build_changer_data',
  inherit = study_pop_library,
  public = list(
    
    base_year = NULL,
    change_year = NULL,
    drug_class = NULL,
    figure_type = '',
    base_data = data_frame(),
    change_data = data_frame(),
    combined_data = data_frame(),
    tidy_data = data_frame(),
    
    initialize = function(base_year = '2013', change_year = '2014', drug_class = 'statins',
                          figure_type = '') {
      self$base_year <- base_year
      self$change_year <- change_year
      self$drug_class <- drug_class
      self$figure_type <- figure_type
    },
    
    read_base_year_source = function(year = self$base_year) {
      self$base_data <- Kmisc::kLoad(paste0('data/source_tables/study_group_', self$drug_class,
                                            '_', year, '.rData')) %>% 
        select(NPI, 
               base_year_class_claims = total_class_claims,
               base_year_target_claims = total_target_claims, 
               base_year_payments = total_target_payment_number,
               base_year_bene_count = doc_bene_count) %>% 
        mutate(base_year = year)
    },
    
    read_change_year_source = function(year = self$change_year) {
      self$change_data <- Kmisc::kLoad(paste0('data/source_tables/study_group_', self$drug_class,
                                            '_', year, '.rData')) %>% 
        select(NPI,
               doc_state,
               doc_specialty,
               change_year_class_claims = total_class_claims,
               change_year_target_claims = total_target_claims, 
               change_year_payments = total_target_payment_number,
               change_year_bene_count = doc_bene_count) %>% 
        mutate(change_year = year)
    },
    
    merge_source_data = function() {
      self$combined_data <- self$base_data %>% 
        inner_join(self$change_data, by = 'NPI') %>% 
        arrange(NPI)
    },
    
    tidy_combined_data = function(type = self$figure_type) {
      
      self$tidy_data <- self$combined_data %>% 
        mutate(
          paid_group = ifelse(is.na(base_year_payments) & is.na(change_year_payments), 'No Meals',
                              ifelse(!is.na(base_year_payments) & is.na(change_year_payments), 'Base Year Meal',
                                     ifelse(is.na(base_year_payments) & !is.na(change_year_payments), 'Change Year Meal',
                                            'Both Year Meals')))
        ) %>% 
        tidyr::gather(key = 'year_vars', value = 'year', base_year, change_year,
                      -base_year_class_claims, -base_year_target_claims,
                      -change_year_class_claims, -change_year_target_claims, 
                      -base_year_bene_count, -change_year_bene_count, 
                      -base_year_payments, -change_year_payments) %>%
        group_by(NPI, year, paid_group) %>%
        summarise(target_claims = ifelse(year == self$base_year,
                                         base_year_target_claims, change_year_target_claims),
                  class_claims = ifelse(year == self$base_year,
                                        base_year_class_claims, change_year_class_claims),
                  bene_count = ifelse(year == self$base_year,
                                      base_year_bene_count, change_year_bene_count),
                  payment_count = ifelse(year == self$base_year,
                                         base_year_payments, change_year_payments)) %>%
        ungroup() %>%
        mutate(
          target_per_bene = target_claims/(bene_count/1000),
          payment_count = ifelse(payment_count %in% NA, 0, payment_count)
        ) 
      
      if (type != 'scatter') {
        self$tidy_data <- self$tidy_data %>% 
          group_by(year, paid_group) %>%
          summarise(
            group_count = n(),
            total_claims = sum(target_claims, na.rm = TRUE),
            total_class_claims = sum(class_claims, na.rm = TRUE),
            mean_prescribing_rate = round(mean(target_claims), 1),
            mean_target_per_bene = round(mean(target_per_bene), 1)
          )
      } else {
        self$tidy_data <- self$tidy_data %>% 
          arrange(NPI, year) %>% 
          group_by(NPI, paid_group) %>% 
          summarise(
            delta_target_claims = diff(target_claims),
            delta_class_claims = diff(class_claims),
            delta_bene_count = diff(bene_count),
            delta_payment_count = diff(payment_count),
            delta_target_per_bene = diff(target_per_bene)
          )
      }
      
    },
    
    save_tables = function() {
      change_data <- self$tidy_data
      save(change_data, file = paste0('data/source_tables/change_data_', self$drug_class, '.rData'))
    },
    
    build_tables = function() {
      self$read_base_year_source()
      self$read_change_year_source()
      self$merge_source_data()
      self$tidy_combined_data()
      if(!(self$figure_type %in% '')) {
        figure <- build_figures$new(data = self$tidy_data, 
                                    class = self$drug_class, 
                                    type = self$figure_type)
        figure$build_figure_data()
        figure$build_figure()
      }
      self$save_tables()
    }
    
  )
)
