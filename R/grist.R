#' Generate a New Api Object
#'
#' @description
#' The `grist_api` generator creates a new object, which is the class containing
#' simple methods to call an api Grist. The class is based on the [R6][R6::R6Class].
#' This work follows the Grist API references https://support.getgrist.com/api/
#' from July 2025, and is based on the Python module grist_api
#' https://github.com/gristlabs/py_grist_api/ and the Linux R package,
#' gristr https://forge.ird.fr/phim/cunnac/gristr
#'
#' ## Initialization
#' A new 'grist_api'-object is initialized using the `new()` method on the generator:
#'
#' \code{
#'  api <- grist_api$new(
#'    server = 'https://grist.numerique.gouv.fr',
#'    api_key = Sys.getenv("GRIST_KEY"),
#'    doc_id = Sys.getenv("GRIST_DOC_TEST")
#'  )
#' }
#'
#'
#' @importFrom R6 R6Class
#' @importFrom httr2 request req_perform req_auth_bearer_token resp_status_desc resp_body_json resp_body_string req_body_raw req_body_json req_method
#' @importFrom dplyr select ungroup transmute rowwise mutate row_number rename_with case_when
#' @importFrom tidyr unnest_wider nest
#' @importFrom tibble tibble enframe
#' @importFrom tidyselect all_of
#' @importFrom purrr list_flatten
#' @importFrom jsonlite fromJSON minify toJSON
#' @importFrom utils URLencode
#' @importFrom assertthat assert_that
#' @importFrom magrittr %<>%
#'
#' @export
#'
#' @examples
#' # Working only if Env Var "GRIST_KEY" and "GRIST_DOC_TEST" are defined
#'
#' # Create a New Api Object on a single grist document
#' api <- grist_api$new(
#'   server = 'https://grist.numerique.gouv.fr',
#'   api_key = Sys.getenv("GRIST_KEY"),
#'   doc_id = Sys.getenv("GRIST_DOC_TEST")
#' )
#'

grist_api <- R6::R6Class("grist_api",
  public = list(
    #' @description Create a new `grist_api` object
    #' @param server string. IP or URL with protocol (http or https)
    #' @param api_key string. your Grist token
    #' @param doc_id string. the internal doc ref in setting > API
    #' @returns A new 'grist_api'-object
    initialize = function(server = NULL, api_key = NULL, doc_id = NULL) {
      stopifnot("missing `server` argument"= is.null(server) == FALSE)
      stopifnot("missing `api_key` argument"= is.null(api_key) == FALSE)
      stopifnot("missing `doc_id` argument"= is.null(doc_id) == FALSE)
      server <- ifelse(substr(server, nchar(server), nchar(server)) == "/", substr(server, 1, nchar(server) - 1), server)
      private$server <- server
      private$api_key <- api_key
      private$doc_id <- doc_id
      stopifnot("Connection failed, please check your settings!"= private$testrequest() == TRUE)
    },
    #' @description print the doc metadata of `grist_api` object
    #' @param ... Passed...
    print = function(...) {
      # print the doc metadata, without private ;)
      print("-----------------")
      print("grist_api print")
      print("-----------------")
      print("<Document info>")
      print(private$baseReq %>% httr2::req_perform() %>% httr2::resp_body_json())
      print("-----------------")
      print("<Public methods>")
      print(names(get(class(self)[1])$public_methods))
    },
    #' @description Low-level interface that returns the REST call result as json
    #' @param url The part of the URL after api/doc/`doc_id` or other prefix. OBLIGATORY
    #' @param json_data A JSON string with records to patch, put, delete or post. Default NULL
    #' @param method Http method to use ('GET', 'POST', 'DELETE', 'PUT', 'PATCH'). Default 'GET' or 'POST' with data
    #' @param prefix To use a call other than  api/doc/`doc_id`. Default NULL
    #' @param type_resp Set the output type string or list. Default "list"
    #' @returns JSON string or list()
    call = function(url, json_data = NULL, method = NULL, prefix = NULL, type_resp = "list") {
      prefix <- ifelse(is.null(prefix), paste0("/api/docs/",private$doc_id), prefix)
      prefix <- ifelse(substr(prefix, nchar(prefix), nchar(prefix)) == "/", substr(prefix, 1, nchar(prefix) - 1), prefix)
      method <- ifelse(is.null(method), ifelse(is.null(json_data), 'GET', 'POST'), method)
      url <- ifelse(substr(url, 1,1) == "/", url, paste0("/", url))
      full_url <- paste0(private$server, prefix,url)
      req <- httr2::request(full_url)
      req %<>% httr2::req_auth_bearer_token(private$api_key)
      req %<>% httr2::req_method(method)
      if(!is.null(json_data)) {
        if(is.integer(json_data)) {
          req %<>% httr2::req_body_json(data = json_data, auto_unbox = FALSE)
        } else {
          req %<>% httr2::req_body_raw(json_data, type = "application/json")
        }
      }
      status <- "Undefined"
      try({
        req %<>% httr2::req_perform()
        status <- req %>% httr2::resp_status_desc()
      }, silent = TRUE)
      if (identical(status, "OK")) {
        if(type_resp == "list") {
          return(req %>% httr2::resp_body_json())
        } else {
          return(req %>% httr2::resp_body_string())
        }
      } else {
        message(paste0("API response: ", status))
        return("KO")
      }
    },
    #' @description Convert a Grist json response string to a table
    #' @param json_data A JSON string. OBLIGATORY
    #' @param structure The name of the Grist response encapsulating structure. Usually "tables", "records". OBLIGATORY
    #' @returns A [tibble::tibble()] object.
    json_to_table = function(json_data, structure) {
      if(!is.null(json_data)) {
        if(json_data == "KO") {
          return(json_data)
        } else {
          res <- json_data %>%
            jsonlite::fromJSON(simplifyVector = FALSE, simplifyDataFrame = FALSE, flatten = FALSE)
          tableContent <- res[[structure]]
          if(length(tableContent) == 0L) {
            message("No records found... Returning a NULL value.")
            tableContent <- NULL
          } else {
            tableContent %<>% tibble::enframe(name = "idx", value = structure) %>%
              tidyr::unnest_wider(tidyselect::all_of(structure), simplify = TRUE, strict = FALSE) %>%
              tidyr::unnest_wider(tidyselect::all_of("fields"), simplify = TRUE, strict = FALSE) %>%
              dplyr::select(-"idx")
          }
          return(tableContent)
        }
      } else {
        return(NULL)
      }
    },
    #' @description Convert a data.frame of records to a json string
    #' @param records A data.frame()
    #' @param mode "add", "modify" or "sync".
    #' @param key_cols A vector of columns (grist_col_id) constituting a unique key. only for "sync" mode
    #' @importFrom rlang .data
    #' @returns A JSON string
    table_to_json = function(records, mode = "modify", key_cols = NULL) {
      if (mode == "sync" | mode == "add") {records %<>% dplyr::mutate(id = dplyr::row_number())}
      jsonrecs <- records %>%
        tidyr::nest(.by = "id", .key = "fields") %>%
        dplyr::rowwise()
      if (mode == "add") {
        jsonrecs %<>% dplyr::transmute(records = list(
          list(
            fields = lapply(.data$fields, \(col) {if (is.list(col)) purrr::list_flatten(col) else col})
          )
        ))
      } else if (mode == "modify") {
        jsonrecs %<>% dplyr::transmute(records = list(
          list(
            id = .data$id,
            fields = lapply(.data$fields, \(col) {if (is.list(col)) purrr::list_flatten(col) else col})
          )
        ))
      } else if (mode == "sync") {
        reqlist <- records %>% dplyr::select(tidyselect::any_of(c("id", key_cols))) %>%
          tidyr::nest(.by = "id", .key = "fields") %>%
          dplyr::rowwise()
        jsonrecs %<>% dplyr::transmute(records = list(
          list(
            require = lapply(reqlist$fields[[id]], \(col) {if (is.list(col)) purrr::list_flatten(col) else col}),
            fields = lapply(.data$fields, \(col) {if (is.list(col)) purrr::list_flatten(col) else col})
          )
        ))
      } else {
        print("Mode must be either \"add\", \"modify\" or \"sync\"")
      }
      jsonrecs %<>%
        dplyr::ungroup() %>%
        as.list() %>%
        jsonlite::toJSON(auto_unbox = TRUE, null = "null") %>%
        jsonlite::minify()
      return(jsonrecs)
    }

  ),

  private = list(
    server = NULL,
    api_key = NULL,
    doc_id = NULL,
    baseReq = NULL,
    perform = NULL,
    tempres = NULL,
    testrequest = function(){
      private$baseReq <- httr2::request(paste0(private$server,"/api/docs/",private$doc_id))
      private$baseReq %<>% httr2::req_auth_bearer_token(private$api_key)
      result <- tryCatch({
        private$baseReq %>% httr2::req_perform()
        perform <- TRUE
      }, error = function(e) {
        message("Erreur : ", e$message)
      }, finally = {
        private$perform <- exists("perform")
      })
      return(private$perform)
    },
    createtablejson = function(table_id, record_dicts){
      # def columns
      factor_columns <- sapply(record_dicts, is.factor)
      record_dicts[factor_columns] <- lapply(record_dicts[factor_columns], as.character)
      cols <- data.frame(id = colnames(record_dicts), label = colnames(record_dicts),
                         type = sapply(sapply(record_dicts, class), function(x){x[1]}))
      cols %<>% dplyr::mutate(type = dplyr::case_when(
        type == "numeric" ~ "Numeric",
        type == "integer" ~ "Int",
        type == "logical" ~ "Bool",
        type == "POSIXct" ~ "DateTime",
        type == "Date"    ~ "Date",
        TRUE              ~ "Text"))
      # create json table
      jsoncols <- cols %>%
        tidyr::nest(.by = "id", .key = "fields") %>%
        dplyr::rowwise() %>%
        dplyr::transmute(columns = list(
          list(
            id = .data$id,
            fields = lapply(.data$fields, \(col) {if (is.list(col)) purrr::list_flatten(col) else col})
          )
        ))
      newtable <- data.frame(id = c(table_id), jsoncols) %>%
        tidyr::nest(.by = "id", .key = "columns") %>%
        dplyr::rowwise() %>%
        dplyr::transmute(tables = list(
          c(id = .data$id,
            columns = as.vector(unname(.data$columns)))
          )) %>%
        dplyr::ungroup() %>%
        as.list() %>%
        jsonlite::toJSON(auto_unbox = TRUE, null = "null") %>%
        jsonlite::minify()
      newtable <- gsub("columns.NA", "columns", newtable)
      return(newtable)
    }
  ),
  lock_class = TRUE
)
