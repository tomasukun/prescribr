#' Builder class for figures
#' @import dplyr
#' @importFrom plyr mapvalues
#' @import stringr
#' @import tidyr
#' @import readr
#' @import ggplot2

build_figures <- R6::R6Class(
  'build_figures',
  inherit = build_changer_data,
  public = list(
    type = '',
    figure_data = data_frame(),
    drug_class = '',
    target_formulary_name = '',
    target_brand_name = '',
    
    initialize = function(data, class, type = NULL) {
      self$figure_data <- data
      self$type <- type
      self$drug_class <- class
    },
    
    build_figure_data = function(data = NULL, type = self$type) {
      # figure meta data
      self$target_formulary_name = plyr::mapvalues(self$drug_class,
                                              names(self$figure_drug_class_formulary),
                                              self$figure_drug_class_formulary)
      self$target_brand_name = plyr::mapvalues(self$drug_class,
                                          names(self$figure_drug_class_brand),
                                          self$figure_drug_class_brand)
      self$figure_data$paid_group <- factor(self$figure_data$paid_group, 
                                            levels = c('None', '2013 Only', 
                                                       '2014 Only', '2013 and 2014'))
      if(self$type == 'script-rate') {
        self$figure_data <- self$figure_data %>%
          select(year, paid_group, mean_prescribing_rate) 
      } else if(self$type == 'per-bene') {
        self$figure_data <- self$figure_data %>%
          select(year, paid_group, mean_target_per_bene) %>%
          tidyr::spread(year, mean_target_per_bene) %>%
          mutate(
            change = `2014` - `2013`,
            percent_diff = round(100*(`2014` - `2013`)/`2013`)
          )
      } else if(self$type %in% c('scatter', 'slopes', 'class-slopes')) {
        self$figure_data <- self$figure_data
      } else{
        cat(sprintf('%s is not a supported figure type \n\n figure data not created', self$type))
      }
      
    },
    
    build_figure = function() {
      if(self$type == 'script-rate') {
        figure <- ggplot2::ggplot(self$figure_data, 
                                   aes(x = paid_group, y = mean_prescribing_rate,
                                       colour = year)) +
          geom_line(aes(group = year), linetype = 2, size = 1.5) + 
          geom_point(size = 1) + 
          geom_hline(yintercept = 0, col = "black", lwd = 0.1) +
          ylab(sprintf("%s (%s \U00AE) Prescribing Rate \n", 
                       self$target_formulary_name, self$target_brand_name)) + 
          xlab("") +
          scale_y_continuous(limits = c(0, ceiling(max(self$figure_data$mean_prescribing_rate))),
                                        breaks = seq(0, ceiling(max(self$figure_data$mean_prescribing_rate)), 2), 
                                        expand = c(0,0)) +
          scale_x_discrete(labels = c('None', '2013 Only', '2014 Only', 'Both Years')) +
          theme(axis.text = element_text(face = "bold", size = 17, colour = "black")) +
          theme(panel.background = element_rect(colour = "white", fill = "white")) +
          theme(axis.line = element_line(colour = "black")) +
          theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(), 
                panel.grid.minor.y = element_blank()) +
          theme(panel.grid.major.y = element_line(colour = "white")) +
          theme(axis.ticks = element_line(colour = "black")) + 
          theme(axis.title = element_text(hjust = 0.5)) +
          theme(legend.key.size = unit(2, "cm"),
                legend.text = element_text(size = 12)) + 
          ggtitle(sprintf("%s", self$target_brand_name)) +
          theme(title = element_text( size = 18, color = "black", hjust = 0.5, face = "bold"))
      } else if(self$type == 'per-bene') {
        figure <- ggplot2::ggplot(self$figure_data,
                                   aes(x = paid_group, y = change)) +
          geom_bar(width = 0.25, fill = 'grey80', stat = 'identity') +
          geom_hline(yintercept = 0, col = "black", lwd = 0.1) +
          geom_hline(yintercept = c(seq(-10, 13), seq(-10, 13)), col = "white", lwd = 0.6) +
          ylab(sprintf("Change in %s (%s \U00AE) Prescribing Rate per 1000 Beneficiaries \n",
                       self$target_formulary_name, self$target_brand_name)) +
          xlab("") +
          scale_y_continuous(limits = c(-10, 13), breaks = seq(-10, 13, 1), expand = c(0,0)) +
          scale_x_discrete(labels = c('None', '2013 Only', '2014 Only', 'Both Years')) +
          theme(axis.text = element_text(face = "bold", size = 17, colour = "black")) +
          theme(panel.background = element_rect(colour = "white", fill = "white")) +
          theme(axis.line = element_line(colour = "black")) +
          theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()) +
          theme(panel.grid.major.y = element_line(colour = "white")) +
          theme(axis.ticks = element_line(colour = "black")) +
          theme(axis.title = element_text(hjust = 0.5)) +
          ggtitle(sprintf("%s", self$target_brand_name)) +
          theme(title = element_text( size = 18, color = "black", hjust = 0.5, face = "bold"))
      } else if(self$type == 'scatter') {
        figure <- ggplot2::ggplot(self$figure_data) + 
          ggplot2::geom_jitter(ggplot2::aes(x = delta_payment_count, 
                                            y = delta_target_per_bene,
                                            colour = paid_group),
                               size = 1.5, alpha = 0.99, width = 0.6, height = 0.5) + 
          scale_colour_brewer(palette = 'Spectral') + 
          geom_hline(yintercept = 0, col = "black", lwd = 0.1) + 
          ylab(sprintf("Difference in %s (%s \U00AE) Prescribing Rate per 1000 Beneficiaries \n",
                       self$target_formulary_name, self$target_brand_name)) +
          xlab("\nDifference in Payments Recieved") +
          scale_y_continuous(limits = c(-400, 700), breaks = seq(-400, 700, 100), expand = c(0,0)) +
          scale_x_continuous(limits = c(-8, 15), breaks = seq(-10, 15, 5), expand = c(0,0)) +
          theme(axis.text = element_text(face = "bold", size = 17, colour = "black")) +
          theme(panel.background = element_rect(colour = "white", fill = "white")) +
          theme(axis.line = element_line(colour = "black")) +
          theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()) +
          theme(panel.grid.major.y = element_line(colour = "white")) +
          theme(axis.ticks = element_line(colour = "black")) +
          theme(axis.title = element_text(hjust = 0.5)) +
          theme(legend.key.size = unit(2, "cm"),
                legend.text = element_text(size = 12)) +
          ggtitle(sprintf("%s", self$target_brand_name)) +
          theme(title = element_text( size = 18, color = "black", hjust = 0.5, face = "bold"))
      } else if(self$type == 'slopes') {
        annotate_text <- self$figure_data %>%
          select(year, paid_group, mean_target_per_bene) %>%
          tidyr::spread(year, mean_target_per_bene) %>%
          mutate(
            change = `2014` - `2013`,
            percent_diff = round(100*(`2014` - `2013`)/`2013`, 1))
        figure <- ggplot2::ggplot(self$figure_data, 
                                  aes(x = year, y = mean_target_per_bene, colour = paid_group)) + 
          geom_line(aes(group = paid_group), linetype = 1, size = 1.5) +  
          geom_point(size = 3, fill = 'white') +
          annotate("text", x = 2.15, y = annotate_text$`2014`, 
                   label = str_c(annotate_text$percent_diff, '%'), size = 12) +
          scale_colour_brewer(palette = 'Spectral') +
          ylab(sprintf("%s (%s \U00AE) Prescribing Rate per 1000 Beneficiaries \n",
                       self$target_formulary_name, self$target_brand_name)) +
          xlab("") +
          scale_y_continuous(limits = c(95, 225), breaks = seq(95, 225, 10), expand = c(0,0)) +
          theme(axis.text = element_text(face = "bold", size = 17, colour = "black")) +
          theme(panel.background = element_rect(colour = "white", fill = "white")) +
          theme(axis.line = element_line(colour = "black")) +
          theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()) +
          theme(panel.grid.major.y = element_line(colour = "white")) +
          theme(axis.ticks = element_line(colour = "black")) +
          theme(axis.title = element_text(hjust = 0.5)) +
          theme(legend.key.size = unit(2, "cm"),
                legend.text = element_text(size = 12, face = "bold")) +
          guide_legend(title = 'Receipt of Payment') +
          ggtitle(sprintf("%s", self$target_brand_name)) +
          theme(title = element_text( size = 18, color = "black", hjust = 0.5, face = "bold"))
      } else if(self$type == 'class-slopes') {
        annotate_text <- self$figure_data %>%
          select(year, paid_group, class_per_bene) %>%
          tidyr::spread(year, class_per_bene) %>%
          mutate(
            change = `2014` - `2013`,
            percent_diff = round(100*(`2014` - `2013`)/`2013`, 1))
        figure <- ggplot2::ggplot(self$figure_data, 
                                  aes(x = year, y = class_per_bene, colour = paid_group)) + 
          geom_line(aes(group = paid_group), linetype = 1, size = 1.5) +  
          geom_point(size = 3, fill = 'white') +
          annotate("text", x = 2.15, y = annotate_text$`2014`, 
                   label = str_c(annotate_text$percent_diff, '%'), size = 12) +
          scale_colour_brewer(palette = 'Spectral') +
          ylab(sprintf("%s (%s \U00AE) Prescribing Rate per 1000 Beneficiaries \n",
                       self$target_formulary_name, self$target_brand_name)) +
          xlab("") +
          scale_y_continuous(limits = c(95, 225), breaks = seq(95, 225, 10), expand = c(0,0)) +
          theme(axis.text = element_text(face = "bold", size = 17, colour = "black")) +
          theme(panel.background = element_rect(colour = "white", fill = "white")) +
          theme(axis.line = element_line(colour = "black")) +
          theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
                panel.grid.minor.y = element_blank()) +
          theme(panel.grid.major.y = element_line(colour = "white")) +
          theme(axis.ticks = element_line(colour = "black")) +
          theme(axis.title = element_text(hjust = 0.5)) +
          theme(legend.key.size = unit(2, "cm"),
                legend.text = element_text(size = 12, face = "bold")) +
          guide_legend(title = 'Receipt of Payment') +
          ggtitle(sprintf("%s", self$target_brand_name)) +
          theme(title = element_text( size = 18, color = "black", hjust = 0.5, face = "bold"))
      } else {
        stop('specify correct figure type')
      }
      browser()
      jpeg(filename = paste0(self$shared_docs_dir, 'figure_changer_analysis_', 
                             self$drug_class, '_', self$type, '.jpeg'),
           width = 1150, height = 1275, quality = 100, units = "px", pointsize = 12)
      figure
      dev.off()
    }
  )
)