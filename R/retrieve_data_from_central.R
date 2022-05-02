#' Retrieve data from central
#'
#' Retrieve data from Central server
#' @param fids Form IDs for which data should be retrieved. If NULL, all
#' @param clean_column_names Whether to clean the column names
#' @return A list
#' @export
#' @import ruODK
#' @import yaml
#' @import dplyr

retrieve_data_from_central <- function(fids = NULL,
                                       clean_column_names = TRUE){

  # Make sure environment variables are sufficient
  environment_variables <- Sys.getenv()
  ok <- 'bohemia_credentials' %in% names(environment_variables)
  if(!ok){
    stop('You need to define a bohemia_credentials environment variable. Do this by runnning credentials_check("path/to/bohemia_credentials.yaml")')
  }
  bohemia_credentials <- Sys.getenv('bohemia_credentials')

  # Actually read in the credentials
  creds <- yaml::yaml.load_file(bohemia_credentials)

  # Set up some parameters
  ruODK::ru_setup(
    fid = NULL,
    # fid = 'ntd',
    url = creds$url,
    un = creds$un,
    pw = creds$pw,
    verbose = TRUE,
    tz = 'UTC'
  )
  project_name <- creds$project_name

  # List projects on the server
  projects <- ruODK::project_list()

  # Define which project to use
  pid <- projects$id[projects$name == project_name]
  ruODK::ru_setup(pid = pid)

  # # Get a list of forms in the project
  # fl <- get_form_list()
  fl <- ruODK::form_list()

  # Cut down to only the form IDs which are relevant
  if(!is.null(fids)){
    fl <- fl %>% filter(fid %in% fids)
  }
  if(nrow(fl) > 0){

    # Loop through each form ID and get the submission
    out_list <- list()
    for(i in 1:nrow(fl)){
      this_fid <- fl$fid[i]
      message('Form ', i, ' of ', nrow(fl), ': ', this_fid)

      # # Get the schema for the form
      # schema <- form_schema_ext(fid = this_fid)
      # schema_df <- schema %>% dplyr::select(ruodk_name, name, type)

      # New zip method
      td <- paste0('/tmp/odk/')
      if(dir.exists(td)){
        unlink(td, recursive = TRUE)
      }
      dir.create(td)
      ruODK::submission_export(
        local_dir = td,
        overwrite = TRUE,
        media = FALSE,
        repeats = TRUE,
        fid = this_fid,
        verbose = TRUE
      )
      # unzip the downloaded files
      ed <- paste0(td, 'unzipped/')
      zip_path <- paste0(td, this_fid, '.zip')
      unzip(zipfile = zip_path, exdir = ed)

      # Read in the downloaded files
      file_names <- dir(ed)
      data_list <- list()
      fid_list <- c()
      for(f in 1:length(file_names)){
        this_file_name <- file_names[f]
        this_sub_form <- gsub('.csv', '', this_file_name)
        # See if this is the main submission form or not
        is_main <- !grepl('-', this_sub_form)
        this_sub_form <- unlist(lapply(strsplit(this_sub_form, split = '-'), function(x){x[length(x)]}))
        if(is_main){
          this_sub_form <- 'Submissions'
        }
        fid_list <- c(fid_list, this_sub_form)
        file_path <- paste0(ed, this_file_name)
        this_data <- readr::read_csv(file_path, guess_max = Inf)
        # Clean the column names
        if(clean_column_names){
          names(this_data) <- unlist(lapply(strsplit(names(this_data), '-'), function(a){a[length(a)]}))
        }
        this_data$id <- this_data$KEY
        this_data <- janitor::clean_names(this_data)
        this_data <- this_data[,!duplicated(names(this_data))]
        data_list[[f]] <- this_data
      }
      names(data_list) <- fid_list
      out_list[[i]] <- data_list
    }
    names(out_list) <- fl$fid

    data_list <- out_list
    message('Returning a list of length ', length(data_list))
    return(data_list)
  } else {
    message('There are no forms with the IDs supplied. Returning an empty list')
    return(list())
  }
}
