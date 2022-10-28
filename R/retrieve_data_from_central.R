#' Retrieve data from central
#'
#' Retrieve data from Central server
#' @param fids Form IDs for which data should be retrieved. If NULL, all
#' @param except_fids Form IDs to exclude from retrieval
#' @param clean_column_names Whether to clean the column names
#' @param handle_sefull_split Whether to smartly handle the sefull split by combining all the sefull forms into one
#' @return A list
#' @export
#' @import ruODK
#' @import yaml
#' @import dplyr
#' @import data.table

retrieve_data_from_central <- function(fids = NULL,
                                       except_fids = NULL,
                                       clean_column_names = TRUE,
                                       handle_sefull_split = TRUE){

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

  # Redefine project_list since ruODK function broke
  isodt_to_local <- function(datetime_string,
                             orders = c("YmdHMS", "YmdHMSz"),
                             tz = get_default_tz()) {
    datetime_string %>%
      lubridate::parse_date_time(orders = orders) %>%
      lubridate::with_tz(., tzone = tz)
  }
  get_project_list <- function (url = get_default_url(), un = get_default_un(), pw = get_default_pw(), 
                                retries = get_retries(), orders = c("YmdHMS", "YmdHMSz", 
                                                                    "Ymd HMS", "Ymd HMSz", "Ymd", "ymd"), tz = get_default_tz()) {
    require(ruODK)
    # yell_if_missing(url, un, pw)
    httr::RETRY("GET", httr::modify_url(url, path = glue::glue("v1/projects")), 
                httr::add_headers(Accept = "application/xml", `X-Extended-Metadata` = "true"), 
                httr::authenticate(un, pw), times = retries) %>% 
      # yell_if_error(., url, un, pw) %>%
      httr::content(.) %>% tibble::tibble(.) %>% 
      tidyr::unnest_wider(".", names_repair = "universal") %>% 
      janitor::clean_names(.) %>% dplyr::mutate_at(dplyr::vars("last_submission", 
                                                               "created_at", 
                                                               "updated_at"#, 
                                                               # "deleted_at" # this is the only change
      ), ~isodt_to_local(., 
                         orders = orders, tz = tz)) %>% {
                           if ("archived" %in% names(.)) {
                             dplyr::mutate(., archived = tidyr::replace_na(archived, 
                                                                           FALSE))
                           }
                           else {
                             .
                           }
                         }
  }

  
  # List projects on the server
  # projects <- ruODK::project_list()
  projects <- get_project_list()

  # Define which project to use
  pid <- projects$id[projects$name == project_name]
  ruODK::ru_setup(pid = pid)

  # # Get a list of forms in the project
  # fl <- get_form_list()
  # fl <- ruODK::form_list() # this function stopped working in newer versions,
  # replacing here
  gfl  <- function(pid = get_default_pid(),
                   url = get_default_url(),
                   un = get_default_un(),
                   pw = get_default_pw(),
                   retries = get_retries(),
                   orders = c(
                     "YmdHMS",
                     "YmdHMSz",
                     "Ymd HMS",
                     "Ymd HMSz",
                     "Ymd",
                     "ymd"
                   ),
                   tz = get_default_tz()) {
    httr::RETRY(
      "GET",
      httr::modify_url(url, path = glue::glue("v1/projects/{pid}/forms")),
      httr::add_headers(
        "Accept" = "application/xml",
        "X-Extended-Metadata" = "true"
      ),
      httr::authenticate(un, pw),
      times = retries
    ) %>%
      httr::content(.) %>%
      tibble::tibble(.) %>%
      tidyr::unnest_wider(".", names_repair = "universal") %>%
      # tidyr::unnest_wider(
      #   "reviewStates",
      #   names_repair = "universal", names_sep = "_"
      # ) %>%
      # tidyr::unnest_wider(
      #   "createdBy",
      #   names_repair = "universal", names_sep = "_"
      # ) %>%
      janitor::clean_names() %>%
      dplyr::mutate_at(
        dplyr::vars(dplyr::contains("_at")), # assume datetimes are named "_at"
        ~ isodt_to_local(., orders = orders, tz = tz)
      ) %>%
      dplyr::mutate(fid = xml_form_id)
  }
  fl <- gfl()

  
  # Cut down to only the form IDs which are relevant
  if(!is.null(fids)){
    fl <- fl %>% filter(fid %in% fids)
  }
  # Remove the except ones
  if(!is.null(except_fids)){
    message('Not retrieving any data for: ', except_fids)
    fl <- fl %>% filter(!fid %in% except_fids)
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
    
    # Handle the sefull split
    if(handle_sefull_split){
      message('Handling the sefull split')
      if('sefullv1' %in% names(out_list) & 'sefull' %in% names(out_list)){
        for(j in 1:length(names(out_list$sefull))){
          message(j)
          this_name <- names(out_list$sefull)[j]
          
          df5 <- out_list$sefull[[this_name]]
          df1 <- out_list$sefullv1[[this_name]]
          df2 <- out_list$sefullv2[[this_name]]
          df3 <- out_list$sefullv3[[this_name]]
          df4 <- out_list$sefullv4[[this_name]]
          
          if('anc_when' %in% names(df3)){
            fix_anc_when <- function(d){
              d %>% mutate(anc_when = as.Date(anc_when))
            }
            df1 <- df1 %>% fix_anc_when()
            df2 <- df2 %>% fix_anc_when()
            df3 <- df3 %>% fix_anc_when()
            df4 <- df4 %>% fix_anc_when()
            df5 <- df5 %>% fix_anc_when()
          }
          if('drug_swallow_date' %in% names(df3)){
            fix_drug_swallow_date <- function(d){
              d %>% mutate(drug_swallow_date = as.Date(drug_swallow_date))
            }
            df1 <- df1 %>% fix_drug_swallow_date()
            df2 <- df2 %>% fix_drug_swallow_date()
            df3 <- df3 %>% fix_drug_swallow_date()
            df4 <- df4 %>% fix_drug_swallow_date()
            df5 <- df5 %>% fix_drug_swallow_date()
          }
          if('drug_swallow_date_alt' %in% names(df3)){
            fix_drug_swallow_date_alt <- function(d){
              d %>% mutate(drug_swallow_date_alt = as.POSIXct(drug_swallow_date_alt, tz = 'Europe/Madrid'))
            }
            df1 <- df1 %>% fix_drug_swallow_date_alt()
            df2 <- df2 %>% fix_drug_swallow_date_alt()
            df3 <- df3 %>% fix_drug_swallow_date_alt()
            df4 <- df4 %>% fix_drug_swallow_date_alt()
            df5 <- df5 %>% fix_drug_swallow_date_alt()
          }
          if('irs_past12_check' %in% names(df5)){
            fix_irs_past12_check <- function(d){
              d %>% mutate(irs_past12_check = as.Date(irs_past12_check))
            }
            df1 <- df1 %>% fix_irs_past12_check()
            df2 <- df2 %>% fix_irs_past12_check()
            df3 <- df3 %>% fix_irs_past12_check()
            df4 <- df4 %>% fix_irs_past12_check()
            df5 <- df5 %>% fix_irs_past12_check()
          }
          if('sec8_q1_dob_1' %in% names(df3)){
            fix_sec8_q1_dob_1 <- function(d){
              d %>% mutate(sec8_q1_dob_1 = as.POSIXct(sec8_q1_dob_1, tz = 'Europe/Madrid'))
            }
            df1 <- df1 %>% fix_sec8_q1_dob_1()
            df2 <- df2 %>% fix_sec8_q1_dob_1()
            df3 <- df3 %>% fix_sec8_q1_dob_1()
            df4 <- df4 %>% fix_sec8_q1_dob_1()
            df5 <- df5 %>% fix_sec8_q1_dob_1()
          }
          if('sec8_q1_dob_2' %in% names(df3)){
            fix_sec8_q1_dob_2 <- function(d){
              d %>% mutate(sec8_q1_dob_2 = as.POSIXct(sec8_q1_dob_2, tz = 'Europe/Madrid'))
            }
            df1 <- df1 %>% fix_sec8_q1_dob_2()
            df2 <- df2 %>% fix_sec8_q1_dob_2()
            df3 <- df3 %>% fix_sec8_q1_dob_2()
            df4 <- df4 %>% fix_sec8_q1_dob_2()
            df5 <- df5 %>% fix_sec8_q1_dob_2()
          }
          if('sec8_q2_dob_1' %in% names(df3)){
            fix_sec8_q2_dob_1 <- function(d){
              d %>% mutate(sec8_q2_dob_1 = as.POSIXct(sec8_q2_dob_1, tz = 'Europe/Madrid'))
            }
            df1 <- df1 %>% fix_sec8_q2_dob_1()
            df2 <- df2 %>% fix_sec8_q2_dob_1()
            df3 <- df3 %>% fix_sec8_q2_dob_1()
            df4 <- df4 %>% fix_sec8_q2_dob_1()
            df5 <- df5 %>% fix_sec8_q2_dob_1()
          }
          if('sec8_q2_dob_2' %in% names(df3)){
            fix_sec8_q2_dob_2 <- function(d){
              d %>% mutate(sec8_q2_dob_2 = as.POSIXct(sec8_q2_dob_2, tz = 'Europe/Madrid'))
            }
            df1 <- df1 %>% fix_sec8_q2_dob_2()
            df2 <- df2 %>% fix_sec8_q2_dob_2()
            df3 <- df3 %>% fix_sec8_q2_dob_2()
            df4 <- df4 %>% fix_sec8_q2_dob_2()
            df5 <- df5 %>% fix_sec8_q2_dob_2()
          }
          
          
          
          x <-
            tibble(data.table::rbindlist(
              list(df1, df2, df3, df4, df5)
            ))
          out_list$sefull[[this_name]] <- x
        }
      }
      out_list$sefullv1 <- NULL
      out_list$sefullv2 <- NULL
      out_list$sefullv3 <- NULL
      out_list$sefullv4 <- NULL
    }

    data_list <- out_list
    message('Returning a list of length ', length(data_list))
    return(data_list)
  } else {
    message('There are no forms with the IDs supplied. Returning an empty list')
    return(list())
  }
}
