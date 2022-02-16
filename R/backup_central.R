#' Backup Central
#'
#' Create a backup of the entirety of the Central server
#' @param out_file The path of the file for writing the zip to
#' @return A list
#' @export
#' @import ruODK
#' @import yaml
#' @import dplyr
#' @import httr

backup_central <- function(out_file = 'backup.zip'){

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

  # Define a function for retrieving a session token
  central_url = creds$url
  central_email = creds$un
  central_password = creds$pw
  r <- httr::POST(
      url = paste0(central_url, '/v1/sessions'),
      body = list('email' = central_email, 'password' = central_password),
      encode = 'json')
  if(r$status_code == 200){
    session_token <- httr::content(r)
  } else {
    stop('Failure in token retrieval')
  }


  # Use the token to get the full backup
  r <- httr::POST(
    url = paste0(central_url, '/v1/backup'),
    add_headers(Authorization = paste("Bearer", session_token$token, sep = " ")),
    body = list('passphrase' = creds$backup_pw),
    encode = 'json',
    write_disk(path = out_file, overwrite = TRUE))
  if(!r$status_code == 200){
    stop('Failure in zip retrieval')
  }
  # Indicate that the task is finished and return the response object
  message('File written to ', out_file)
  return(r)
}
