#' Builder for changer analysis, comparing two years of prescribing and payments
#' @import dplyr
#' @importFrom plyr mapvalues
#' @import stringr
#' @import tidyr
#' @import readr
#' @import purrr
#' @import lubridate
#' @import ggplot2
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
    
    tidy_combined_data = function() {
      
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
                      -change_year_class_claims, -change_year_target_claims, 
                      -base_year_bene_count, -change_year_bene_count) %>%
        group_by(NPI, year, paid_group) %>%
        summarise(target_claims = ifelse(year == self$base_year,
                                        base_year_target_claims,
                                        change_year_target_claims),
                  class_claims = ifelse(year == self$base_year,
                                        base_year_class_claims,
                                        change_year_class_claims),
                  bene_count = ifelse(year == self$base_year,
                                      base_year_bene_count,
                                      change_year_bene_count)) %>%
        ungroup() %>%
        mutate(
          target_per_bene = target_claims/(bene_count/1000)
        ) %>% 
        group_by(year, paid_group) %>%
        summarise(
          group_count = n(),
          total_claims = sum(target_claims, na.rm = TRUE),
          total_class_claims = sum(class_claims, na.rm = TRUE),
          mean_prescribing_rate = round(mean(target_claims), 1),
          mean_target_per_bene = round(mean(target_per_bene), 1)
        ) %>% 
        mutate(
          target_rate = round(100*(total_claims/total_class_claims), 1)
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
        browser()
        # figure_data <- self$tidy_data %>%
        #   select(year, paid_group, mean_target_per_bene) %>%
        #   tidyr::spread(year, mean_target_per_bene) %>%
        #   mutate(
        #     change = `2014` - `2013`,
        #     percent_diff = round(100*(`2014` - `2013`)/`2013`)
        #   )
        figure_data <- self$tidy_data %>%
          select(year, paid_group, mean_prescribing_rate) 
        annotation_data <- figure_data %>% 
          tidyr::spread(year, mean_prescribing_rate) %>%
          mutate(
            change = `2014` - `2013`,
            percent_diff = round(100*(`2014` - `2013`)/`2013`)
          )
        figure_data$paid_group <- factor(figure_data$paid_group, 
                                         levels = c('No Meals', 'Base Year Meal', 
                                                    'Change Year Meal', 'Both Year Meals'))
        # figure meta data
        target_formulary_name = plyr::mapvalues(self$drug_class,
                                         names(self$figure_drug_class_formulary),
                                         self$figure_drug_class_formulary)
        target_brand_name = plyr::mapvalues(self$drug_class,
                                      names(self$figure_drug_class_brand),
                                      self$figure_drug_class_brand)
        
        # figure1 <- ggplot2::ggplot(figure_data, 
        #                            aes(x = paid_group, y = change)) +
        #   geom_bar(width = 0.25, fill = 'grey80', stat = 'identity') + 
        #   geom_hline(yintercept = 0, col = "black", lwd = 0.1) +
        #   geom_hline(yintercept = c(seq(-8,-1), seq(1,8)), col = "white", lwd = 0.6) +
        #   ylab(sprintf("Change in %s (%s \U00AE) Prescribing Rate per 1000 Beneficiaries \n", 
        #                target_formulary_name, target_brand_name)) + 
        #   xlab("") +
        #   scale_y_continuous(limits = c(-8, 8), breaks = seq(-8, 8, 1), expand = c(0,0)) +
        #   scale_x_discrete(labels = c('None', '2013 Only', '2014 Only', 'Both Years')) +
        #   theme(axis.text = element_text(face = "bold", size = 17, colour = "black")) +
        #   theme(panel.background = element_rect(colour = "white", fill = "white")) +
        #   theme(axis.line = element_line(colour = "black")) +
        #   theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
        #         panel.grid.minor.y = element_blank()) +
        #   theme(panel.grid.major.y = element_line(colour = "white")) +
        #   theme(axis.ticks = element_line(colour = "black")) + 
        #   theme(axis.title = element_text(hjust = 0.5)) +
        #   ggtitle(sprintf("%s", target_brand_name)) +
        #   theme(title = element_text( size = 18, color = "black", hjust = 0.5, face = "bold"))
        
        figure1 <- ggplot2::ggplot(figure_data, 
                                   aes(x = paid_group, y = change)) +
          geom_bar(width = 0.25, fill = 'grey80', stat = 'identity') + 
          geom_hline(yintercept = 0, col = "black", lwd = 0.1) +
          geom_hline(yintercept = c(seq(-8,-1), seq(1,8)), col = "white", lwd = 0.6) +
          # annotate("text",
          #          label = percent_increase$increase,
          #          x = percent_increase$x_pos,
          #          y = percent_increase$y_pos,
          #          size = 7) +
          ylab(sprintf("Change in %s (%s \U00AE) Prescribing Rate per 1000 Beneficiaries \n", 
                       target_formulary_name, target_brand_name)) + 
          xlab("") +
          scale_y_continuous(limits = c(-8, 8), breaks = seq(-8, 8, 1), expand = c(0,0)) +
          scale_x_discrete(labels = c('None', '2013 Only', '2014 Only', 'Both Years')) +
          theme(axis.text = element_text(face = "bold", size = 17, colour = "black")) +
          theme(panel.background = element_rect(colour = "white", fill = "white")) +
          theme(axis.line = element_line(colour = "black")) +
          theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
                panel.grid.minor.y = element_blank()) +
          theme(panel.grid.major.y = element_line(colour = "white")) +
          theme(axis.ticks = element_line(colour = "black")) + 
          theme(axis.title = element_text(hjust = 0.5)) +
          ggtitle(sprintf("%s", target_brand_name)) +
          theme(title = element_text( size = 18, color = "black", hjust = 0.5, face = "bold"))
        # jpeg(filename = paste0(self$shared_docs_dir, 'figure1_changer_analysis_', self$drug_class, '.jpeg'), 
        #      width = 1250, height = 1150, quality = 100, units = "px", pointsize = 12)
      }
      self$save_tables()
    }
    
  )
)
