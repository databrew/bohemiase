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
      # Get info about the groups in each form
      sub_forms <-
        odata_service_get(
          pid = get_default_pid(),
          fid = this_fid,
          url = get_default_url(),
          un = get_default_un(),
          pw = get_default_pw(),
          retries = get_retries()
        )
      # Get the schema for the form
      schema <- form_schema_ext(fid = this_fid)
      schema_df <- schema %>% dplyr::select(ruodk_name, name, type)

      # Loop through each group and retrieve the table
      fid_list <- list()
      for(j in 1:nrow(sub_forms)){
        this_sub_form <- sub_forms$name[j]
        # Define which part of column names needs to be removed
        # in order to make column names concordant
        remove_this <- gsub('Submissions.', '', this_sub_form, fixed = TRUE)
        remove_this <- paste0(remove_this, '_')
        message('---sub-form ', j, ' of ', nrow(sub_forms), ': ', this_sub_form)
        # Get all the submissions for the sub form in question
        this_data <- ruODK::odata_submission_get(
          table = this_sub_form, #fq_svc$name[1],
          fid = this_fid,
          wkt=TRUE)
        # Clean the column names
        if(clean_column_names){
          columns_df <- tibble(ruodk_name = names(this_data))
          right <- schema_df %>%
            mutate(ruodk_name = gsub(remove_this, '', ruodk_name))
          columns_df <- left_join(columns_df,
                                  right,
                                  by = "ruodk_name") %>%
            mutate(name = ifelse(is.na(name), ruodk_name, name))
          names(this_data) <- columns_df$name
        }
        this_data <- this_data[,!duplicated(names(this_data))]
        fid_list[[j]] <- this_data
      }
      names(fid_list) <- sub_forms$name
      out_list[[i]] <- fid_list
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
