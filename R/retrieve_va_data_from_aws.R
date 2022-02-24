#' Retrieve VA data from AWS
#'
#' Retrieve VA data from the project's AWS S3 bucket. This is not for individual VA forms, but rather for the aggregated VA data as aggregated in the "pipeline" code
#' @return A list
#' @export
#' @import aws.s3
#' @import yaml
#' @import dplyr

retrieve_va_data_from_aws <- function(){

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

  # Define prefix
  prefix <- paste0('va/')

  # Get the contents matching the prefix
  buck <- get_bucket(bucket = 'bohemia2022',
                     prefix = prefix,
                     max = Inf)
  if(length(buck) > 0){
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
  } else {
    message('No data matching the form IDs in question. Returning an empty list.')
    return(list())
  }

}
