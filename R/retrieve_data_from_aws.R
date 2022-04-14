#' Retrieve data from AWS
#'
#' Retrieve ODK Central data from the project's AWS S3 bucket
#' @param fid Form ID for which data should be retrieved. If NULL, all
#' @param central Whether to use the Central Server (by default, TRUE) or the aggregate server
#' @param clean Whether to retrieve clean data or not (by default, FALSE)
#' @return A list
#' @export
#' @import aws.s3
#' @import yaml
#' @import dplyr

retrieve_data_from_aws <- function(fid = NULL, central = TRUE, clean = FALSE){

  if(is.null(fid)){
    if(clean){
      stop('No functionality built yet for retrieving "clean" data for all data; you must instead supply a fid if you want clean data for a specific form.')
    }
  }

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

  if(central){
    server_name <- gsub('https://', '', creds$url, fixed = TRUE)
    project_name <- creds$project_name
  } else {
    server_name <- gsub('https://', '', creds$agg_url, fixed = TRUE)
    project_name <- 'Aggregate'
  }


  # Retrieve for a specific form
  if(!is.null(fid)){
    prefix <- paste0(paste0(server_name, '/',
                            project_name, '/',
                            fid, '/'))
  } else {
    # No specific form identified; return the "data list"
    if(clean){
      message('No functionality yet for retrieving "clean" data for more than one form at a time; if you want "clean" data, supply a fid')
    }
    prefix <- paste0(server_name, '/',
                     project_name, '/202')
  }
  # Get the contents matching the prefix
  clean_available <- FALSE
  if(clean){
    # If clean, keep only those which are clean
    buck <- get_bucket(bucket = 'bohemia2022',
                       prefix = paste0(prefix, 'clean'),
                       max = Inf)
    if(length(buck) > 0){
      clean_available <- TRUE
    } else {
      message('No "clean" data availale for ', fid, '. Retrieving raw data instead.')
    }
  }
  if(!clean_available){
    buck <- get_bucket(bucket = 'bohemia2022',
                       prefix = prefix,
                       max = Inf)
  }


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
    if(!clean){
      nr0 <- nrow(buck_df)
      buck_df <- buck_df %>% filter(!grepl('clean', buck_names))
      nr1 <- nrow(buck_df)
      if(nr1 < nr0){
        n <- nr0 - nr1
        message('---Ignoring the ', n, ' "clean" versions of ', fid, ' and instead returning the most recent raw version')
      }
    }
    # Get most recent object
    most_recent <- buck_df %>%
      filter(grepl('.RData', file)) %>%
      filter(date_time == max(date_time, na.rm = TRUE)) %>%
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
