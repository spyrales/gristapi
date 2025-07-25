% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_api.R
\name{delete_records}
\alias{delete_records}
\title{Delete rows in a table}
\usage{
delete_records(api, table_id, record_ids)
}
\arguments{
\item{api}{A 'gristapi::grist_api'-object}

\item{table_id}{Normalized table name (see \code{id} in \code{listtables} method). First character upper. OBLIGATORY}

\item{record_ids}{A vector of integers corresponding the id of the records to be deleted. OBLIGATORY}
}
\value{
\code{TRUE} if the transaction was successful.
}
\description{
Deletes records from the given table. The data is a vector of record IDs.
}
\examples{
# delete data in a given table
table_name <- paste0("Mtcars_",get_os())
api <- grist_api$new(
  server = 'https://grist.numerique.gouv.fr',
  api_key = Sys.getenv("GRIST_KEY"),
  doc_id = Sys.getenv("GRIST_DOC_TEST")
)
delete_records(api, table_name, as.integer(c(6)))

}
\seealso{
Other function_gristapi: 
\code{\link{add_records}()},
\code{\link{countrows}()},
\code{\link{fetch_table}()},
\code{\link{listcolumns}()},
\code{\link{listtables}()},
\code{\link{sync_table}()},
\code{\link{update_records}()}
}
\concept{function_gristapi}
