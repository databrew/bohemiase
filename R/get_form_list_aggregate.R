#' Get form list from aggregate
#'
#' Retrieve a dataframe of forms on the Aggregate server
#' @param pre_auth Whether to pre-authenticate or not (some Aggregate versions require it)
#' @return A list
#' @export
#' @import httr
#' @import yaml
#' @import dplyr
#' @import xml2

get_form_list_aggregate <-
  function (pre_auth = FALSE){

    # Make sure environment variables are sufficient
    environment_variables <- Sys.getenv()
    ok <- 'bohemia_credentials' %in% names(environment_variables)
    if(!ok){
      stop('You need to define a bohemia_credentials environment variable. Do this by runnning credentials_check("path/to/bohemia_credentials.yaml")')
    }
    bohemia_credentials <- Sys.getenv('bohemia_credentials')
    creds <- yaml::yaml.load_file(bohemia_credentials)

    user <- creds$agg_un
    password <- creds$agg_pw
    url <- creds$agg_url
  if (is.null(user) | is.null(password)) {
    message("No user/password were entered. Will try with it. If the server requires it, you'll get a 401 error")
  }
  if (pre_auth) {
    auth_url <- paste0(url, "/local_login.html?redirect=formList")
    r <- POST(auth_url, authenticate(user = user, password = password,
                                     type = "basic"))
  }
  fl_url <- paste0(url, "/formList")
  if (is.null(user)) {
    r <- GET(fl_url)
  }
  else {
    r <- GET(fl_url, authenticate(user = user, password = password,
                                  type = "digest"))
  }
  stop_for_status(r)
  warn_for_status(r)
  message_for_status(r)
  contingut <- content(r)
  xname <- xml_name(contingut)
  if (xname != "forms") {
    stop("Something went wrong. Tried to fetch forms, but instead got ",
         xname)
  }
  xnens <- xml_children(contingut)
  form_names <- xml_text(xnens)
  urls <- xml_attr(xnens, "url")
  get_id <- function(x) {
    out <- strsplit(x, "formId=", fixed = TRUE)
    out <- lapply(out, function(z) {
      z[2]
    })
    out <- unlist(out)
    return(out)
  }
  ids <- get_id(urls)
  out <- tibble(name = form_names, id = ids, url = urls)
  return(out)
}
