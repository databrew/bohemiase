#' Recode values
#'
#' Given a list of data created by bohemiase::retrieve_data_from_aws and a "schema" table created by bohemiase::get_form_schema, recode values to their human-readable equivalents
#' @param data_list A list of data, like sefull$data
#' @param schema_table A dataframe of the form schema
#' @param language The language to be used. By default, "english"
#' @param variables_too Whether to recode variable names too. By default, FALSE
#' @param messaging Whether to show messages
#' @param exclude_variables A vector of variables to exclude from recoding (ie, those which are "select_multiple". Default: none)
#' @return A dataframe
#' @export
#' @import dplyr

recode_values <- function(data_list, schema_table, language = 'english', variables_too = FALSE, messaging = TRUE, exclude_variables = c()){

  # Loop through each element of data list and recode choices
  table_names <- names(data_list)
  for(i in 1:length(table_names)){
    this_table_name <- table_names[i]
    if(messaging){
      message('Recoding ',i, ' of ', length(table_names), ': ', this_table_name)
    }
    this_data <- data_list[[this_table_name]]
    # Loop through each column
    for(j in 1:ncol(this_data)){
      this_column_name <- names(this_data)[j]
      if(messaging){
        message('---Column ', j, ' of ', ncol(this_data), ': ', this_column_name)
      }
      this_schema_row <- schema_table %>%
        filter(name == this_column_name)
      if(nrow(this_schema_row) == 1){
        # Recode variable name if applicable
        if(variables_too){
          variable_name_var <- paste0('label_', language)
          if(variable_name_var %in% names(this_schema_row)){
            new_variable_name <- unlist(this_schema_row[,variable_name_var])
            if(!is.na(new_variable_name)){
              names(this_data)[j] <- new_variable_name
            }
          }
        }
        # Recode the variables if applicable
        if(nrow(this_data) > 0){
          if(!this_column_name %in% exclude_variables){
            # Get the choices
            choices_var <- paste0('choices_', language)
            if(choices_var %in% names(this_schema_row)){
              the_choices <- this_schema_row %>% dplyr::select(one_of(choices_var)) %>% pull
              if(!is.null(unlist(the_choices))){
                if(!is.na(unlist(the_choices))[1]){
                  joiner <- as.data.frame(the_choices) %>%
                    mutate(values = as.character(values)#,
                           # %>% as.characterlabels = as.character(labels)
                           )
                  out_vector <-
                    tibble(values = this_data[,j] %>% pull %>% as.character) %>%
                    left_join(joiner)  %>%
                    mutate(labels = ifelse(is.na(labels), values, labels)) %>%
                    dplyr::select(labels) %>%
                    pull
                  this_data[,j] <- out_vector
                }
              }
            }
          }
        }
      }
    }
    data_list[[i]] <- this_data
  }
  return(data_list)
}
