#' Retrieve data from AWS
#'
#' Retrieve data from the project's AWS S3 bucket
#' @param fid Form ID for which data should be retrieved. If NULL, all
#' @return A list
#' @export
#' @import aws.s3
#' @import yaml
#' @import dplyr

retrieve_data_from_aws <- function(fid = NULL){

  if(!is.null(fid)){
    if(length(fid) > 1){
      stop('fid must be either NULL to get a list of all data, or length 1 for just one form; it cannot be more than one')
    }
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

  # AWS credentials setup
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = creds$aws_access_key_id,
    "AWS_SECRET_ACCESS_KEY" = creds$aws_secret_access_key,
    "AWS_DEFAULT_REGION" = "eu-west-3"
  )

  server_name <- gsub('https://', '', creds$url, fixed = TRUE)
  project_name <- creds$project_name

  # Retrieve for a specific form
  if(!is.null(fid)){
    prefix <- paste0(paste0(server_name, '/',
                            project_name, '/',
                            fid, '/'))
  } else {
    # No specific form identified; return the "data list"
    prefix <- paste0(server_name, '/',
                     project_name, '/202')
  }
  # Get the contents matching the prefix
  buck <- get_bucket(bucket = 'bohemia2022',
                     prefix = prefix,
                     max = Inf)
  # Get the time/date for each object
  buck_names <- buck_times <-  c()
  for(i in 1:length(buck)){
    buck_names[i] <- buck[i]$Contents$Key
    buck_times[i] <- buck[i]$Contents$LastModified
  }
  # Reorganize as a dataframe
  buck_df <- tibble(file = buck_names,
                    date_time = substr(buck_times, 1, 19)) %>%
    mutate(date_time = as.POSIXct(date_time, format = '%Y-%m-%dT%H:%M:%OS'))
  # Get most recent object
  most_recent <- buck_df %>%
    filter(grepl('.RData', file)) %>%
    filter(date_time == max(date_time)) %>%
    dplyr::sample_n(1)
  # Retrieve the actual object
  aws.s3::s3load(
    object = most_recent$file,
    bucket = 'bohemia2022'
  )
  return(list(data = robject, time = most_recent$date_time))
}
