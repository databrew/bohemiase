#' Retrieve data from aggregate
#'
#' Retrieve data from Aggregate server
#' @param fids Form IDs for which data should be retrieved. If NULL, all
#' @return A list
#' @export
#' @import yaml
#' @import dplyr
#' @import odkr
#' @import readr

retrieve_data_from_aggregate <- function(fids = NULL){

  # Make sure environment variables are sufficient
  environment_variables <- Sys.getenv()
  ok <- 'bohemia_credentials' %in% names(environment_variables)
  if(!ok){
    stop('You need to define a bohemia_credentials environment variable. Do this by runnning credentials_check("path/to/bohemia_credentials.yaml")')
  }
  bohemia_credentials <- Sys.getenv('bohemia_credentials')

  # Actually read in the credentials
  creds <- yaml::yaml.load_file(bohemia_credentials)

  # Get a briefcase jar
  work_dir <- getwd()
  if(!'odkBriefcase_latest.jar' %in% dir()){
    odkr::get_briefcase(destination = work_dir)
  }

  # Define some credentials
  server <- creds$agg_url
  user <- creds$agg_un
  password <- creds$agg_pw

  # Get the form list
  fl <- get_form_list_aggregate(pre_auth = F)

  # Cut down to only the form IDs which are relevant
  if(!is.null(fids)){
    fl <- fl %>% filter(id %in% fids)
  }
  if(nrow(fl) < 1){
    stop('There are no forms with the IDs supplied')
  }

  # Loop through each form ID and get the submission
  # but first delete anything already there
  # Remove the ODK Briefcase folder
  unlink('ODK Briefcase Storage', recursive = TRUE)
  unlink('csvs', recursive = TRUE)

  # Create a folder for csvs
  if(!'csvs' %in% dir()){
    dir.create('csvs')
  }
  out_list <- list()
  for(i in 1:nrow(fl)){
    id <- this_fid <- fl$id[i]
    message('Form ', i, ' of ', nrow(fl), ': ', this_fid)

    pull_remote(target = work_dir,
                id = id,
                to = work_dir,
                from = server,
                username = user,
                password = password)

    export_data(target = work_dir,
                id = id,
                from = work_dir,
                to = 'csvs',
                filename = paste0(id, '.csv'))
  }

  #reading csv files
  odk_data <- list()
  counter <- 0
  ids <- fl$id
  for(i in 1:length(ids)){
    this_id <- ids[i]
    sub_forms <- dir('csvs')[grepl(paste0(ids[i], ''), dir('csvs'))]
    sub_forms <- sub_forms[grepl('.csv', sub_forms)]
    this_form_list <- list()
    # Get the parent form
    parent_file <- paste0(this_id, '.csv')
    this_form_list$Submissions <- read_csv((paste0('csvs/', parent_file)))
    # If applicable, get the repeats too
    sub_forms <- sub_forms[!sub_forms %in% parent_file]
    if(length(sub_forms) > 0){
      for(j in 1:length(sub_forms)){
        file_name <- sub_forms[j]
        list_name <- gsub('.csv', '', file_name)
        list_name <- gsub(paste0(this_id, '-'), '', list_name, fixed = TRUE)
        this_form_list[[list_name]] <- read_csv(file=paste0('csvs/', file_name))
      }
    }
    odk_data[[i]] <- this_form_list
  }

  names(odk_data) <- fl$id
  message('Returning a list of length ', length(odk_data))
  return(odk_data)
}
