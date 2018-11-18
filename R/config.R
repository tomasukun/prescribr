fetch = function(var) {
  value = Sys.getenv(var)

  if(value == '') stop(var, ' is not set. Add to .Renviron')
  value
}

source_dir = function()    fetch("SOURCE_DIR")
processed_dir = function() fetch("PROCESSED_DIR")
shared_dir = function()    fetch('SHARED_DIR')
