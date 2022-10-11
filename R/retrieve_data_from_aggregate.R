#' Retrieve data from aggregate
#'
#' Retrieve data from Aggregate server
#' @param fids Form IDs for which data should be retrieved. If NULL, all
#' @param except_fids Form IDs to exclude from retrieval
#' @param use_local_briefcase Whether to use the local briefcase directory
#' @param start_fresh Whether to start data retrieval fresh (TRUE) or use already existant xml files in the briefcase directory (FALSE)
#' @param handle_duplicates Smartly handle entoscreeningb and entof2f3f4f5 forms, which have multiples with the same title
#' @return A list
#' @export
#' @import yaml
#' @import dplyr
#' @import odkr
#' @import readr

retrieve_data_from_aggregate <- function(fids = NULL,
                                         except_fids = NULL,
                                         use_local_briefcase = TRUE,
                                         start_fresh = FALSE,
                                         handle_duplicates = TRUE){
  
  owd <- getwd()
  
  # Make sure environment variables are sufficient
  environment_variables <- Sys.getenv()
  ok <- 'bohemia_credentials' %in% names(environment_variables)
  if(!ok){
    stop('You need to define a bohemia_credentials environment variable. Do this by runnning credentials_check("path/to/bohemia_credentials.yaml")')
  }
  bohemia_credentials <- Sys.getenv('bohemia_credentials')
  
  # Actually read in the credentials
  creds <- yaml::yaml.load_file(bohemia_credentials)
  briefcase_directory <- creds$briefcase_directory
  
  # Get a briefcase jar
  if(!'odkBriefcase_latest.jar' %in% dir(briefcase_directory)){
    odkr::get_briefcase(destination = briefcase_directory)
  }
  
  # Define some credentials
  server <- creds$agg_url
  user <- creds$agg_un
  password <- creds$agg_pw
  
  # Get the form list
  fl <- get_form_list_aggregate(pre_auth = F)
  fl <- fl %>% arrange(id)
  
  # Go into briefcase directory
  setwd(briefcase_directory)
  message('New working directory is ', briefcase_directory)
  
  # Cut down to only the form IDs which are relevant
  if(!is.null(fids)){
    fl <- fl %>% filter(id %in% fids)
  }
  # Remove the except ones
  if(!is.null(except_fids)){
    message('Not retrieving any data for: ', except_fids)
    fl <- fl %>% filter(!fid %in% except_fids)
  }
  if(nrow(fl) < 1){
    stop('There are no forms with the IDs supplied')
  }
  
  # Loop through each form ID and get the submission
  # Remove the ODK Briefcase folder
  # unlink('ODK Briefcase Storage', recursive = TRUE)
  if(file.exists('briefcase.log')){
    file.remove('briefcase.log')
  }
  out_list <- list()
  odk_data <- list()
  counter <- 0
  ids <- fl$id
  for(i in 1:nrow(fl)){
    # first delete anything already there
    unlink('csvs', recursive = TRUE)
    # Create a folder for csvs
    if(!'csvs' %in% dir()){
      dir.create('csvs')
    }
    
    id <- this_id <- this_fid <- fl$id[i]
    message('Form ', i, ' of ', nrow(fl), ': ', this_fid)
    
    pull_remote_recent(target = briefcase_directory,
                       id = id,
                       to = briefcase_directory,
                       from = server,
                       username = user,
                       password = password,
                       start_fresh = start_fresh)
    
    export_data(target = briefcase_directory,
                id = id,
                from = briefcase_directory,
                to = 'csvs',
                filename = paste0(id, '.csv'))
    
    # Read csv files
    sub_forms <- dir('csvs')[grepl(paste0(ids[i], ''), dir('csvs')) | dir('csvs') == ids[i]]
    # Due to similar naming in va153, va153b, va153b2, and va153census, these
    # can be mistaken as sub-forms and need to be dealt with appropriately
    sub_forms <- sub_forms[!grepl('va153', sub_forms)]
    # # Similarly with entoscreening and entoscreeningb
    # sub_forms <- sub_forms[!grepl('entoscreeningb|entoscreening', sub_forms)]
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
    # Smartly handle duplicates
    # ie, delete the ODK Briefcase storage directory specifically for forms with problematic names
    if(handle_duplicates){
      unlink(paste0(briefcase_directory, '/ODK Briefcase Storage/forms/Formulário de Triagem de Entomologia'),
             recursive = TRUE)
      unlink(paste0(briefcase_directory, '/ODK Briefcase Storage/forms/Ento Formulário de bioeficacia de ivermectina nos mosquitos de campo'))
    }
  }
  
  names(odk_data) <- fl$id
  message('Returning a list of length ', length(odk_data))
  setwd(owd)
  return(odk_data)
}
