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
    model_type = '',
    base_data = data_frame(),
    change_data = data_frame(),
    combined_data = data_frame(),
    figure_data = data_frame(),
    model_data = data_frame(),
    
    initialize = function(base_year = '2013', change_year = '2014', drug_class = 'statins',
                          figure_type = '', model_type = '') {
      self$base_year <- base_year
      self$change_year <- change_year
      self$drug_class <- drug_class
      self$figure_type <- figure_type
      self$model_type <- model_type
    },
    
    build_tables = function() {
      private$read_base_year_source()
      private$read_change_year_source()
      private$merge_source_data()
      if(!(self$figure_type %in% '')) {
        private$build_figure_data()
        figure <- build_figures$new(data = self$figure_data, class = self$drug_class, type = self$figure_type)
        figure$build_figure_data()
        figure$build_figure()
      }
      if(!(self$model_type %in% '')) {
        private$build_model_data(type = self$model_type)
        analysis <- build_model$new(data = self$model_data, class = self$drug_class, type = self$model_type)
        analysis$run_model()
      }
      self$save_tables()
    }
  ),
  
  private = list(
    read_base_year_source = function() {
      self$base_data <- Kmisc::kLoad(paste0('data/source_tables/study_group_', self$drug_class,
                                            '_', self$base_year, '.rData')) %>% 
        mutate(base_year_class_claims = total_class_claims,
               base_year_target_claims = total_target_claims, 
               base_year_payments = total_target_payment_number,
               base_year_bene_count = doc_bene_count,
               base_year = self$base_year) %>% 
        select(NPI, doc_specialty, doc_state, doc_gender, doc_mapd_claims, doc_lis_claims, 
               doc_group_size, doc_total_claims, doc_grad_year, base_year_class_claims,
               base_year_target_claims, base_year_payments, base_year_bene_count, base_year)
    },
    
    read_change_year_source = function() {
      self$change_data <- Kmisc::kLoad(paste0('data/source_tables/study_group_', self$drug_class,
                                            '_', self$change_year, '.rData')) %>% 
        mutate(change_year_class_claims = total_class_claims,
               change_year_target_claims = total_target_claims, 
               change_year_payments = total_target_payment_number,
               change_year_bene_count = doc_bene_count, 
               change_year = self$change_year) %>% 
        select(NPI, change_year_class_claims, change_year_target_claims,
               change_year_payments, change_year_bene_count, change_year)
    },
    
    merge_source_data = function() {
      self$combined_data <- self$base_data %>% 
        inner_join(self$change_data, by = 'NPI') %>%
        mutate(
          paid_group = ifelse(is.na(base_year_payments) & is.na(change_year_payments), 'None',
                              ifelse(!is.na(base_year_payments) & is.na(change_year_payments), '2013 Only',
                                     ifelse(is.na(base_year_payments) & !is.na(change_year_payments), '2014 Only',
                                            '2013 and 2014')))
        ) %>% 
        tidyr::gather(key = 'year_vars', value = 'year', base_year, change_year,
                      -base_year_class_claims, -base_year_target_claims,
                      -change_year_class_claims, -change_year_target_claims, 
                      -base_year_bene_count, -change_year_bene_count, 
                      -base_year_payments, -change_year_payments) %>% 
        arrange(NPI)
    },
    
    build_figure_data = function(type = self$figure_type) {
      
      self$figure_data <- self$combined_data %>%
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
        filter(!is.na(bene_count)) %>% 
        mutate(target_per_bene = target_claims/(bene_count/1000),
               class_per_bene = class_claims/(bene_count/1000),
               payment_count = ifelse(payment_count %in% NA, 0, payment_count)) 
      
      if (type != 'scatter') {
        self$figure_data <- self$figure_data %>% 
          group_by(year, paid_group) %>%
          summarise(
            group_count = n(),
            total_claims = sum(target_claims, na.rm = TRUE),
            total_class_claims = sum(class_claims, na.rm = TRUE),
            mean_prescribing_rate = round(mean(target_claims), 1),
            mean_target_per_bene = round(mean(target_per_bene), 1),
            mean_class_per_bene = round(mean(class_per_bene), 1)
          )
      } else {
        self$figure_data <- self$figure_data %>% 
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
    
    build_model_data = function(type = self$model_type) {
      if(str_detect(type,'did')) {
        self$model_data <- self$combined_data %>% 
          group_by(NPI, year, doc_specialty, doc_state, doc_gender, doc_total_claims,
                   doc_grad_year, doc_mapd_claims, doc_lis_claims, doc_group_size, paid_group) %>%
          summarise(target_claims = ifelse(year == self$base_year,
                                           base_year_target_claims, change_year_target_claims),
                    class_claims = ifelse(year == self$base_year,
                                          base_year_class_claims, change_year_class_claims),
                    bene_count = ifelse(year == self$base_year,
                                        base_year_bene_count, change_year_bene_count),
                    payment_count = ifelse(year == self$base_year,
                                           base_year_payments, change_year_payments)) %>% 
          ungroup() %>% 
          mutate(payment_count = ifelse(payment_count %in% NA, 0, payment_count),
                 target_per_bene = target_claims/(bene_count/1000)) 
      } else{
        self$model_data <- self$combined_data
      }
    },
    
    save_tables = function() {
      change_data <- self$figure_data
      save(change_data, file = paste0('data/source_tables/change_data_', self$drug_class, '.rData'))
    }
  )
)
