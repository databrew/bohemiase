#' Backup Aggregate
#'
#' Create a backup of the entirety of the Aggregate server's data
#' @param out_file The path of the file for writing the zip to
#' @return A list
#' @export
#' @import odkr
#' @import yaml
#' @import dplyr
#' @import httr

backup_aggregate <- function(out_file = 'backup_aggregate.zip'){

  if(!grepl('.zip', out_file, fixed = TRUE)){
    stop('out_file must be a zip file')
  }

  # Make sure environment variables are sufficient
  environment_variables <- Sys.getenv()
  ok <- 'bohemia_credentials' %in% names(environment_variables)
  if(!ok){
    stop('You need to define a bohemia_credentials environment variable. Do this by runnning credentials_check("path/to/bohemia_credentials.yaml")')
  }
  bohemia_credentials <- Sys.getenv('bohemia_credentials')

  # Actually read in the credentials
  creds <- yaml::yaml.load_file(bohemia_credentials)

  # Define briefcase directory and directory to zip
  briefcase_directory <- creds$briefcase_directory
  directory_to_zip <- file.path(briefcase_directory, 'ODK Briefcase Storage')
  if(!dir.exists(directory_to_zip)){
    stop('No ODK Briefcase Storage directory in ', briefcase_directory)
  }
  zip(zipfile = out_file,
      files = directory_to_zip)

  # Indicate that the task is finished and return the response object
  message('File written to ', out_file)
}

