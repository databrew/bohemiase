#' Get form schema
#'
#' Retrieve a dataframe of a form schema from the Central server
#' @param fid Form ID
#' @return A dataframe
#' @export
#' @import ruODK
#' @import yaml
#' @import dplyr

get_form_schema <- function(fid){
  
  # Make sure environment variables are sufficient
  environment_variables <- Sys.getenv()
  ok <- 'bohemia_credentials' %in% names(environment_variables)
  if(!ok){
    stop('You need to define a bohemia_credentials environment variable. Do this by runnning credentials_check("path/to/bohemia_credentials.yaml")')
  }
  bohemia_credentials <- Sys.getenv('bohemia_credentials')
  
  # Actually read in the credentials
  creds <- yaml::yaml.load_file(bohemia_credentials)
  project_name <- creds$project_name
  
  # Set up some parameters
  ruODK::ru_setup(
    fid = fid,
    url = creds$url,
    un = creds$un,
    pw = creds$pw,
    verbose = TRUE,
    tz = 'UTC'
  )
  # List projects on the server
  projects <- ruODK::project_list()
  
  # Define which project to use
  pid <- projects$id[projects$name == project_name]
  ruODK::ru_setup(pid = pid)
  
  # Get schema
  fqx <- ruODK::form_schema_ext(fid = 'sefull')
  # fqx_flattened <- ruODK::form_schema_ext(fid = 'sefull', flatten = TRUE)
  # fqx_sanitized <- ruODK::form_schema_ext(fid = 'sefull', odata = TRUE)
  # fqx_parsed <- ruODK::form_schema_ext(fid = 'sefull', parse = TRUE)
  return(fqx)
}