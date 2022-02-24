#' Retrieve any arbitrary object from the bohemia2022 bucket
#' Retrieve any arbitrary object from the bohemia2022 bucket
#' @param object Path to object to get
#' @param file Where to save locally
#' @return A list
#' @export
#' @import aws.s3
#' @import yaml
#' @import dplyr

retrieve_any_object_from_aws <- function(object,
                                         file){

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

    # Retrieve the actual object
    aws.s3::save_object(
      object = object,
      bucket = 'bohemia2022',
      file = file
    )


}
