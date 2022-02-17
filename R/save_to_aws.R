#' Save to AWS
#'
#' Save data to AWS
#' @param robject An R object to save. Alternatively, use file_path
#' @param file_path The path to a file to save. Alternatively, use robject
#' @param bucket_path The name of the object to be saved in the bucket (including path and extension)
#' @return Files will be written to AWS
#' @export
#' @import aws.s3
#' @import yaml

save_to_aws <- function(robject = NULL,
                        file_path = NULL,
                        bucket_path = NULL, ...){

  if(is.null(robject) & is.null(file_path)){
    stop('You must name at least an robject or a file_path')
  }
  if(!is.null(robject) & !is.null(file_path)){
    stop('You cannot supply both an robject and file_path argument. Pick just one.')
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

  # Set some environment variables
  Sys.setenv(
    "AWS_ACCESS_KEY_ID" = creds$aws_access_key_id,
    "AWS_SECRET_ACCESS_KEY" = creds$aws_secret_access_key,
    "AWS_DEFAULT_REGION" = "eu-west-3"
  )

  # Save the robject to a file
  if(is.null(file_path)){
    file_path <- tempfile(fileext = '.RData')
    save(robject, file = file_path)
  }

  # Upload to bucket
  aws.s3::put_object(
    file = file_path,
    object = bucket_path,
    bucket = 'bohemia2022',
    multipart = TRUE
  )
  message('Uploaded ', file_path, ' to ', bucket_path)

}
