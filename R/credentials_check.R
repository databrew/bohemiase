#' Credentials check
#'
#' Check on or create credentials. If a credentials_path argument is supplied, this function will set the bohemia_credentials environment variable to that path; if it is not supplied, this function will do the same, but will first prompt a few fields so as to create the credentials file.
#' @param credentials_path Path to a bohemia_credentials.yaml file
#' @return A credentials file will be created and a bohemia_credentials environment variable will be set
#' @export
#' @import yaml

credentials_check <- function(credentials_path = NULL){

  # If a path was given, confirm that it's the path they want
  if(is.null(credentials_path)){
    out <- menu(choices = c('Yes', 'No, stop.'),
                title = 'You did not supply a path to a credentials file. Would you like to make one now?')

    if(out == 1){
      url <- ''
      while(nchar(url) < 1){
        url <- readline(prompt="ODK Central server (include https://): ")
      }

      project_name <- ''
      while(nchar(project_name) < 1){
        project_name <- readline(prompt="ODK Central project_name: ")
      }

      un <- ''
      while(nchar(un) < 1){
        un <- readline(prompt="ODK Central user email: ")
      }

      pw <- ''
      while(nchar(pw) < 1){
        pw <- readline(prompt="ODK Central user password: ")
      }

      backup_pw <- ''
      while(nchar(backup_pw) < 1){
        backup_pw <- readline(prompt="ODK Central password for backups: ")
      }

      aws_access_key_id <- ''
      while(nchar(aws_access_key_id) < 1){
        aws_access_key_id <- readline(prompt="AWS S3 Access key ID: ")
      }

      aws_secret_access_key <- ''
      while(nchar(aws_secret_access_key) < 1){
        aws_secret_access_key <- readline(prompt="AWS S3 Secret access key: ")
      }

      message('Great! You have set up the following credentials.')
      message('---ODK Central server: ', url)
      message('---ODK Central project name: ', project_name)
      message('---ODK Central user: ', un)
      message('---ODK Central pass: ', pw)
      message('---ODK Central backup pass: ', backup_pw)
      message('---AWS S3 Access key ID: ', aws_access_key_id)
      message('---AWS S3 Secrety access key: ', aws_secret_access_key)

      is_ok <- FALSE
      while(!is_ok){
        write_to <- readline(prompt = 'In which folder would you like to write your bohemia_credentials.yaml file (type the folder path)? ')
        is_ok <- dir.exists(write_to)
      }
      yaml_lines <-
        c(paste0('url: ', url),
          paste0('project_name: ', project_name),
          paste0('un: ', un),
          paste0('pw: ', pw),
          paste0('backup_pw: ', backup_pw),
          paste0('AWS_ACCESS_KEY_ID: ', aws_access_key_id),
          paste0('AWS_SECRET_ACCESS_KEY: ', aws_secret_access_key),
          paste0('AWS_SECRET_ACCESS_KEY: "eu-west-3"'),
          c(''))
      out_file <- file.path(write_to, 'bohemia_credentials.yaml')
      conn <- file(out_file)
      writeLines(text = yaml_lines,
                 con = conn)
      close(conn)
      message('Successfully wrote a file to ', out_file)
      credentials_path <- out_file
    } else {
      stop('Okay, stopping.')
    }
  }

  message('Going to set the bohemia_credentials environment variable to ', credentials_path)
  Sys.setenv('bohemia_credentials'=credentials_path)
  message('(done)')
  # env_var <- Sys.getenv('bohemia_credentials')

  # # Do some AWS specific environment variable handling
  # creds <- yaml::yaml.load_file(credentials_path)
  # for(i in 1:length(creds)){
  #   Sys.setenv(names(creds)[i] = as.character(creds[i]))
  # }
  # Sys.setenv(
  #   "AWS_ACCESS_KEY_ID" = awscreds$`Access key ID`,
  #   "AWS_SECRET_ACCESS_KEY" = awscreds$`Secret access key`,
  #   "AWS_DEFAULT_REGION" = "eu-west-3"
  # )
}
