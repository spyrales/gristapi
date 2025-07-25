#' List tables
#' @description List tables in the target Grist document
#' @param api A 'gristapi::grist_api'-object
#' @family function_gristapi
#' @returns A [tibble::tibble()] object.
#' @export
#'
#' @examples
#' # List the tables in the document
#' api <- grist_api$new(
#'   server = 'https://grist.numerique.gouv.fr',
#'   api_key = Sys.getenv("GRIST_KEY"),
#'   doc_id = Sys.getenv("GRIST_DOC_TEST")
#' )
#' listtables(api)
listtables <-  function(api){
  stopifnot("Connection failed, please check your settings api!"= api$.__enclos_env__$private$testrequest() == TRUE)
  #get tables
  api$json_to_table(api$call("/tables", type_resp = "string"), "tables")
}


#' List columns of a table
#' @description Fetch the specs of columns in a target Grist document table
#' @param api A 'gristapi::grist_api'-object
#' @param table_id Normalized table name (see `id` in `listtables` method). First character upper. OBLIGATORY
#' @param hidden Set to TRUE to include the hidden columns. Default FALSE
#' @family function_gristapi
#' @returns A [tibble::tibble()] object.
#' @export
#'
#' @examples
#' # Working only if Env Var "GRIST_KEY" and "GRIST_DOC_TEST" are defined
#' # see the structure of a given table
#' table_name <- paste0("Mtcars_", get_os())
#' api <- grist_api$new(
#'   server = 'https://grist.numerique.gouv.fr',
#'   api_key = Sys.getenv("GRIST_KEY"),
#'   doc_id = Sys.getenv("GRIST_DOC_TEST")
#' )
#' listcolumns(api, table_id = table_name)
#'
listcolumns <- function(api, table_id, hidden = FALSE){
  stopifnot("Connection failed, please check your settings api!"= api$.__enclos_env__$private$testrequest() == TRUE)
  #get columns
  comp <- ifelse(hidden,"?hidden=true","")
  res <- api$json_to_table(api$call(paste0("/tables/", table_id, "/columns", comp), type_resp = "string"), "columns")
  if(typeof(res) == "character" && res == "KO"){
    message("`table_id` is not correctly defined. See result of `listtables` method.")
    return(NULL)
  } else {
    return(res)
  }
}


#' Count rows in a table
#' @description Returns the number of rows in a table
#' @param api A 'gristapi::grist_api'-object
#' @param table_id Normalized table name (see `id` in `listtables` method). First character upper. OBLIGATORY
#' @returns A integer.
#' @family function_gristapi
#' @export
#'
#' @examples
#' # See number of rows
#' table_name <- paste0("Mtcars_", get_os())
#' api <- grist_api$new(
#'   server = 'https://grist.numerique.gouv.fr',
#'   api_key = Sys.getenv("GRIST_KEY"),
#'   doc_id = Sys.getenv("GRIST_DOC_TEST")
#' )
#' countrows(api, table_name)
#'
countrows <- function(api, table_id) {
  stopifnot("Connection failed, please check your settings api!"= api$.__enclos_env__$private$testrequest() == TRUE)
  #get columns
  res <- api$json_to_table(api$call(paste0("/sql?q=select%20count%28%2A%29%20as%20nb%20from%20",table_id), type_resp = "string"), "records")
  if(typeof(res) == "character" && res == "KO"){
    message("`table_id` is not correctly defined. See result of `listtables` method.")
    return(NULL)
  } else {
    return(res$nb)
  }
}


#' Get data from a table
#' @description Fetch all data in the table by the given name, returning a tibble with field
#' names corresponding to the columns in that table.
#' @param api A 'gristapi::grist_api'-object
#' @param table_id Normalized table name (see `id` in `listtables` method). First character upper. OBLIGATORY
#' @param filters String. This is a JSON object mapping column names AND (&) sorting result AND (&) limit number of rows.
#' example : `filter={"Country":["UNITED STATES"],"Card_Member":["CLARE DUDLEY","DARIUS BURGESS"]}&sort=Card_Member,-Date&limit=10`
#' See the [grist API doc](https://support.getgrist.com/api/#tag/records/operation/listRecords) for more examples
#' Note : add `&hidden=true` to include the hidden columns
#' Default NULL
#' @returns A [tibble::tibble()] object.
#' @family function_gristapi
#' @export
#'
#' @examples
#' # view all data from a given table
#' table_name <- paste0("Mtcars_", get_os())
#' api <- grist_api$new(
#'   server = 'https://grist.numerique.gouv.fr',
#'   api_key = Sys.getenv("GRIST_KEY"),
#'   doc_id = Sys.getenv("GRIST_DOC_TEST")
#' )
#' fetch_table(api, table_name)
#'
#' # view data from a given table with a limit
#' fetch_table(api, table_name, filters = "limit=10")
#'
#' # view data from a given table with a filter
#' fetch_table(api, table_name, filters = 'filter={"cyl": [8]}')
#'
fetch_table <- function(api, table_id, filters = NULL) {
  stopifnot("Connection failed, please check your settings api!"= api$.__enclos_env__$private$testrequest() == TRUE)
  #get records
  comp <- ifelse(nchar(filters) > 0, paste0("?", gsub(':', '%3A', URLencode(filters))), "")
  res <- api$json_to_table(api$call(paste0("/tables/", table_id, "/records", comp), type_resp = "string"), "records")
  if(typeof(res) == "character" && res == "KO"){
    message("`table_id` or `filters` are not correctly defined. See results of `listtables` and `listcolumns` methods.")
    return(NULL)
  } else {
    return(res)
  }
}


#' Add rows in a table / Create table
#' @description Adds new records to the given table. The data is a data.frame.
#' @param api A 'gristapi::grist_api'-object
#' @param table_id Normalized table name (see `id` in `listtables` method). First character upper. OBLIGATORY
#' @param record_dicts A data frame holding the records with new values. OBLIGATORY
#' @param create_or_replace boolean. TRUE, the table is created if it does not exist or replaced with the structure of the new records. Default FALSE
#' @importFrom sf st_drop_geometry
#' @importFrom assertthat assert_that
#' @importFrom dplyr rename_with
#' @importFrom magrittr %<>%
#' @importFrom methods is
#' @returns An integer vector of newly created ids.
#' @family function_gristapi
#' @export
#'
#' @examples
#' # Create a table with data
#' table_name <- paste0("Mtcars_",get_os())
#' api <- grist_api$new(
#'   server = 'https://grist.numerique.gouv.fr',
#'   api_key = Sys.getenv("GRIST_KEY"),
#'   doc_id = Sys.getenv("GRIST_DOC_TEST")
#' )
#' add_records( api,
#'   table_id = table_name,
#'   record_dicts = data.frame(names = row.names(mtcars), mtcars),
#'   create_or_replace = TRUE
#' )
#' # Add data in a table
#' library(dplyr)
#' new_row <- fetch_table(api, table_name, filters = 'filter={"names": ["Duster 360"]}')
#' new_row <- new_row |> mutate(names = paste0(names,' - custom'), mpg = 16.7)
#' new_row <- new_row |> select(-id)
#' add_records(api, table_name, new_row)
#'
add_records <- function(api, table_id, record_dicts, create_or_replace = FALSE) {
  stopifnot("Connection failed, please check your settings api!"= api$.__enclos_env__$private$testrequest() == TRUE)
  # post records
  if(is(record_dicts, "sf")){
    message("Geometry has been removed from the data. Objects of type sf are not importable into Grist.")
    record_dicts %<>% sf::st_drop_geometry()
  }
  record_dicts <- as.data.frame(record_dicts)
  assertthat::assert_that(!("id" %in% colnames(record_dicts)), msg = "You should not have an id column in the record_dicts table")
  # grist modif auto
  record_dicts <- dplyr::rename_with(record_dicts, ~ gsub(".", "_" , .x, fixed = TRUE))
  if(create_or_replace){
    substr(table_id, 1, 1) <- toupper(substr(table_id, 1, 1))
    # remove table if exist
    if(table_id %in% listtables(api)$id){
      api$call("/apply/", json_data = paste0('[["RemoveTable", "',table_id,'"]]'), method = 'POST')
    }
    # create table
    api$call("/tables/", json_data = api$.__enclos_env__$private$createtablejson(table_id, record_dicts), method = 'POST')
  } else {
    assertthat::assert_that(all(colnames(record_dicts) %in% listcolumns(api, table_id, hidden = TRUE)$id),
                            msg = "The structure of the record_dicts table does not match the destination table")
  }
  # add records (loop by 500 rows)
  res <- api$call(paste0("/tables/", table_id, "/records"),
                  json_data = api$table_to_json(record_dicts[1:min(500, nrow(record_dicts)), ], mode = "add"), method = 'POST')
  if(typeof(res) == "character" && res == "KO"){api$.__enclos_env__$private$tempres <- res}
  if(nrow(record_dicts) > 500) {
    lapply(seq(501, nrow(record_dicts), by = 500), function(i) {
      if(is.null(api$.__enclos_env__$private$tempres)) {
        j <- as.integer(i + 499)
        j <- ifelse(j < nrow(record_dicts), j, nrow(record_dicts))
        recordsi <- record_dicts[i:j, ]
        res <- api$call(paste0("/tables/",table_id,"/records"), json_data = api$table_to_json(recordsi, mode = "add"), method = 'POST')
        if(typeof(res) == "character" && res == "KO") {api$.__enclos_env__$private$tempres <- res}
      }
    })
  }
  if(!is.null(api$.__enclos_env__$private$tempres)) {
    api$.__enclos_env__$private$tempres <- NULL
    message("`table_id` or `record_dicts` are not correctly defined. See results of `listtables` and `listcolumns` methods.")
    return(NULL)
  } else {
    if(nrow(record_dicts) > 500) {
      message(paste0("Returns only the first 500 ids. ", nrow(record_dicts), " rows have been added to your table. It has a total of ",
                     countrows(api, table_id), " rows."))
    }
    return(res %>% unlist(use.names = FALSE))
  }
}


#' Delete rows in a table
#' @description Deletes records from the given table. The data is a vector of record IDs.
#' @param api A 'gristapi::grist_api'-object
#' @param table_id Normalized table name (see `id` in `listtables` method). First character upper. OBLIGATORY
#' @param record_ids A vector of integers corresponding the id of the records to be deleted. OBLIGATORY
#' @importFrom assertthat assert_that
#' @returns `TRUE` if the transaction was successful.
#' @family function_gristapi
#' @export
#'
#' @examples
#' # delete data in a given table
#' table_name <- paste0("Mtcars_",get_os())
#' api <- grist_api$new(
#'   server = 'https://grist.numerique.gouv.fr',
#'   api_key = Sys.getenv("GRIST_KEY"),
#'   doc_id = Sys.getenv("GRIST_DOC_TEST")
#' )
#' delete_records(api, table_name, as.integer(c(6)))
#'

delete_records <- function(api, table_id, record_ids) {
  stopifnot("Connection failed, please check your settings api!"= api$.__enclos_env__$private$testrequest() == TRUE)
  # post data delete
  assertthat::assert_that(is.integer(record_ids), msg = "record_ids must be an integer vector")
  res <- api$call(paste0("/tables/",table_id,"/data/delete"), json_data = record_ids, method = 'POST')
  if(typeof(res) == "character" && res == "KO") {
    message("`table_id` or `record_ids` are not correctly defined. See result of `listtables` method.")
    }
  if(is.null(res)) return(TRUE) else return(FALSE)
}



#' Update rows in a table
#' @description Update existing records in the given table. The data is a data.frame.
#' @param api A 'gristapi::grist_api'-object
#' @param table_id Normalized table name (see `id` in `listtables` method). First character upper. OBLIGATORY
#' @param record_dicts A data frame holding the records with modified values. OBLIGATORY
#' @returns `TRUE` if the transaction was sucessfull.
#' @importFrom assertthat assert_that
#' @importFrom sf st_drop_geometry
#' @importFrom magrittr %<>%
#' @importFrom methods is
#' @family function_gristapi
#' @export
#'
#' @examples
#' # update data in a given table
#' library(dplyr)
#' table_name <- paste0("Mtcars_", get_os())
#' api <- grist_api$new(
#'   server = 'https://grist.numerique.gouv.fr',
#'   api_key = Sys.getenv("GRIST_KEY"),
#'   doc_id = Sys.getenv("GRIST_DOC_TEST")
#' )
#' row_to_up <- fetch_table(api, table_name, filters = 'filter={"names": ["Merc 280"]}')
#' row_to_up <- row_to_up |> mutate(names = paste0(names,' - car for sale'))
#' update_records(api, table_name, row_to_up)
update_records <- function(api, table_id, record_dicts) {
  stopifnot("Connection failed, please check your settings api!" = api$.__enclos_env__$private$testrequest() == TRUE)
  # patch records
  if(is(record_dicts, "sf")) {
    message("Geometry has been removed from the data. Objects of type sf are not supported in Grist.")
    record_dicts %<>% sf::st_drop_geometry()
  }
  record_dicts <- as.data.frame(record_dicts)
  assertthat::assert_that("id" %in% colnames(record_dicts), msg = "You should have id column in record_dicts table")
  assertthat::assert_that(is.integer(record_dicts$id), msg = "The id column must be an integer vector")
  assertthat::assert_that(all(colnames(record_dicts) %in% c("id", listcolumns(api, table_id, hidden = TRUE)$id)),
                          msg = "The structure of the record_dicts table does not match the destination table")
  # patch records (loop by 500 rows)
  lapply(seq(1, nrow(record_dicts), by = 500), function(i) {
    if(is.null(api$.__enclos_env__$private$tempres)){
      j <- as.integer(i + 499)
      j <- ifelse(j < nrow(record_dicts), j, nrow(record_dicts))
      recordsi <- record_dicts[i:j, ]
      res <- api$call(paste0("/tables/",table_id,"/records"), json_data = api$table_to_json(recordsi), method = 'PATCH')
      if(typeof(res) == "character" && res == "KO") {api$.__enclos_env__$private$tempres <- res}
    }
  })
  if(!is.null(api$.__enclos_env__$private$tempres)){
    api$.__enclos_env__$private$tempres <- NULL
    message("`table_id` or `record_dicts` are not correctly defined. See results of `listtables` and `listcolumns` methods.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' Synchronize a table with data
#' @description Update Grist table with new data, updating existing rows or adding new ones, matching rows on
#' the given key columns. (This method does not remove rows from Grist.)
#' @param api A 'gristapi::grist_api'-object
#' @param table_id Normalized table name (see `id` in `listtables` method). First character upper. OBLIGATORY
#' @param new_data A data frame holding the records with new and modified values. OBLIGATORY
#' @param key_cols A vector of columns (grist_col_id) constituting a unique key
#' @returns `TRUE` if the transaction was sucessfull.
#' @importFrom assertthat assert_that
#' @importFrom sf st_drop_geometry
#' @importFrom magrittr %<>%
#' @importFrom methods is
#' @family function_gristapi
#' @export
#'
#' @examples
#' # Synchronize the table with a dataframe (update or add) on unique key
#' library(dplyr)
#' table_name <- paste0("Mtcars_", get_os())
#' api <- grist_api$new(
#'   server = 'https://grist.numerique.gouv.fr',
#'   api_key = Sys.getenv("GRIST_KEY"),
#'   doc_id = Sys.getenv("GRIST_DOC_TEST")
#' )
#' new_data <- fetch_table(api, table_name, filters = 'filter={"cyl": [8]}')
#' new_data <- new_data |>
#'   mutate(carb = 4, names = ifelse(names == 'Camaro Z28', 'Camaro Z28 - custom', names))
#' new_data <- new_data |> select(-id)
#' # synchronization on the "names" key
#' sync_table(api, table_name, new_data, c("names"))
sync_table <- function(api, table_id, new_data, key_cols) {
  stopifnot("Connection failed, please check your settings api!"= api$.__enclos_env__$private$testrequest() == TRUE)
  # put records
  if(is(new_data, "sf")) {
    message("Geometry has been removed from the data. Objects of type sf are not importable into Grist.")
    new_data %<>% sf::st_drop_geometry()
  }
  new_data <- as.data.frame(new_data)
  assertthat::assert_that(is.character(key_cols), msg = "key_cols must be a character vector")
  assertthat::assert_that(all(key_cols %in% listcolumns(api, table_id, hidden = TRUE)$id),
                          msg = "All elements of key_cols must be columns of the destination table")
  assertthat::assert_that(all(colnames(new_data) %in% listcolumns(api, table_id, hidden = TRUE)$id),
                          msg = "The structure of the new_data table does not match the destination table")
  # put records (loop by 500 rows)
  lapply(seq(1, nrow(new_data), by = 500), function(i) {
    if(is.null(api$.__enclos_env__$private$tempres)) {
      j <- as.integer(i + 499)
      j <- ifelse(j < nrow(new_data), j, nrow(new_data))
      recordsi <- new_data[i:j, ]
      res <- api$call(paste0("/tables/",table_id,"/records"), json_data = api$table_to_json(recordsi, mode = "sync", key_cols = key_cols),
                      method = 'PUT')
      if(typeof(res) == "character" && res == "KO") {api$.__enclos_env__$private$tempres <- res}
    }
  })
  if(!is.null(api$.__enclos_env__$private$tempres)){
    api$.__enclos_env__$private$tempres <- NULL
    message("`table_id` or `new_data` are not correctly defined. See results of `listtables` and `listcolumns` methods.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}
