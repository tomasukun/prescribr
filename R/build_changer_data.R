#' Builder for changer analysis, comparing two years of prescribing and payments
#' @import dplyr
#' @importFrom plyr mapvalues
#' @import stringr
#' @import tidyr
#' @import readr
#' @import purrr
#' @import lubridate
#' @importFrom Kmisc kLoad
 
build_changer_data <- R6::R6Class(
  'build_changer_data',
  inherit = study_pop_library,
  public = list(
    
    base_year = NULL,
    change_year = NULL,
    drug_class = NULL,
    create_figure = NULL,
    base_data = data_frame(),
    change_data = data_frame(),
    combined_data = data_frame(),
    tidy_data = data_frame(),
    
    initialize = function(base_year = '2013', change_year = '2014', drug_class = 'statins',
                          create_figure = FALSE) {
      self$base_year <- base_year
      self$change_year <- change_year
      self$drug_class <- drug_class
      self$create_figure = create_figure
    },
    
    read_base_year_source = function(year = self$base_year) {
      self$base_data <- Kmisc::kLoad(paste0('data/source_tables/study_group_', self$drug_class,
                                            '_', year, '.rData')) %>% 
        select(NPI, 
               base_year_class_claims = total_class_claims,
               base_year_target_claims = total_target_claims, 
               base_year_payments = total_target_payment_number) %>% 
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
               change_year_payments = total_target_payment_number) %>% 
        mutate(change_year = year)
    },
    
    merge_source_data = function() {
      self$combined_data <- self$base_data %>% 
        inner_join(self$change_data, by = 'NPI') %>% 
        arrange(NPI)
    },
    
    tidy_combined_data = function() {
      # self$tidy_data <- self$combined_data %>% 
        # tidyr::gather(key = 'year_vars', value = 'year', base_year, change_year,
        #               -base_year_class_claims, -base_year_target_claims,
        #               -change_year_class_claims, -change_year_target_claims) %>%
        # group_by(NPI, year) %>%
        # summarise(prescribing_rate = ifelse(year == self$base_year,
        #                                     round(base_year_target_claims/base_year_class_claims, 2),
        #                                     round(change_year_target_claims/change_year_class_claims, 2)),
        #           paid = ifelse(year == self$base_year,
        #                         base_year_payments,  change_year_payments)
        # 
        # ) %>%
        # ungroup() %>%
      #   mutate(paid = ifelse(is.na(paid), 'Not Paid', 'Paid')) %>% 
      #   group_by(year, paid) %>% 
      #   summarise(
      #     mean_prescribing_rate = mean(prescribing_rate, na.rm = TRUE),
      #     sd_prescribing_rate = sd(prescribing_rate, na.rm = TRUE),
      #     n = n()
      #   ) %>% 
      #   mutate(
      #     moe = sqrt(mean_prescribing_rate*(1-mean_prescribing_rate)/n)*qnorm(0.975)
      #   )
      
      self$tidy_data <- self$combined_data %>% 
        mutate(
          paid_group = ifelse(is.na(base_year_payments) & is.na(change_year_payments), 'No Meals',
                                   ifelse(!is.na(base_year_payments) & is.na(change_year_payments), 'Base Year Meal',
                                          ifelse(is.na(base_year_payments) & !is.na(change_year_payments), 'Change Year Meal',
                                                 'Both Year Meals')))
        ) %>% 
        select(-base_year_payments, -change_year_payments) %>% 
        tidyr::gather(key = 'year_vars', value = 'year', base_year, change_year,
                      -base_year_class_claims, -base_year_target_claims,
                      -change_year_class_claims, -change_year_target_claims) %>%
        group_by(NPI, year, paid_group) %>%
        summarise(prescribing_rate = ifelse(year == self$base_year,
                                            round(base_year_target_claims/base_year_class_claims, 2),
                                            round(change_year_target_claims/change_year_class_claims, 2))
                  
        ) %>%
        ungroup() %>%
        group_by(year, paid_group) %>%
        summarise(
          group_count = n(),
          p10_prescribing_rate  = quantile(prescribing_rate, probs = 0.1),
          p25_prescribing_rate  = quantile(prescribing_rate, probs = 0.25),
          mean_prescribing_rate = round(100*mean(prescribing_rate), 2),
          p75_prescribing_rate  = quantile(prescribing_rate, probs = 0.75),
          p90_prescribing_rate  = quantile(prescribing_rate, probs = 0.9)
        )
        
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
      if(self$create_figure == TRUE) {
        figure1 <- ggplot2::ggplot(self$tidy_data, 
                                   aes(x = year, y = mean_prescribing_rate, group = paid_group, colour = paid_group)) +
          geom_line(size = 1) +
          geom_point(size = 5, shape = 21, fill = "white") + 
          scale_colour_manual(
                              breaks = c('No Meals', 'Base Year Meal',
                                         'Change Year Meal', 'Both Year Meals'),
                              values = c('No Meals' = '#d4c0ef',
                                         'Base Year Meal' = '#b491e4',
                                         'Change Year Meal' = '#8952d4',
                                         'Both Year Meals' = '#7333cc')
                              ) +
          ylab("Rosuvastatin(Crestor \U00AE) Prescribing Rate among Statins, % \n") + 
          labs(color = "Meal Payment Group") + 
          theme(legend.text = element_text(size = 18)) + 
          theme(legend.title = element_text(size = 18)) + 
          theme(legend.key.size = unit(1.5, "cm")) + 
          scale_y_continuous(limits = c(2, 8), breaks = seq(2, 8, 0.5), expand = c(0,0)) +
          theme(axis.text = element_text(face = "bold", size = 17, colour = "black")) +
          theme(panel.background = element_rect(colour = "#e9e9e9", fill = "#e9e9e9")) +
          theme(axis.line = element_line(colour = "black")) +
          theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
                panel.grid.minor.y = element_blank()) +
          theme(panel.grid.major.y = element_line(colour = "grey80")) +
          theme(axis.ticks = element_line(colour = "black")) + 
          theme(axis.title = element_text(hjust = 0.5)) +
          ggtitle("Rosuvastatin") +
          theme(title = element_text( size = 18, color = "black", hjust = 0.5, face = "bold"))
        browser()
        # jpeg(filename = paste0(self$shared_docs_dir, 'figure1_changer_analysis_', self$drug_class, '.jpeg'), 
        #      width = 1250, height = 1150, quality = 100, units = "px", pointsize = 12)
      }
      self$save_tables()
    }
    
  )
)
