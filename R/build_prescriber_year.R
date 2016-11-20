
build_prescriber_year <- R6::R6Class(
  'build_prescriber_year',
  
  public = list(
    
    year = NULL,
    source_file_dir = '~/Dropbox/physician_payments/',
    
    initialize = function(yr = year) {
      self$year <- yr
    },
    
    read_source_tables = function() {
      return()
    },
    
    process_tables = function() {
      return()
    },
    
    save_processed_tables = function() {
      return()
    }
      
  )
)