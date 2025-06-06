% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api_docs.R
\name{api_docs}
\alias{api_docs}
\alias{api_doc_setting}
\alias{api_doc_add}
\title{Configure your API for serving documentation for itself}
\usage{
api_doc_setting(api, doc_type, doc_path)

api_doc_add(api, doc, overwrite = FALSE, subset = NULL)
}
\arguments{
\item{api}{A plumber2 api object to add docs or doc settings to}

\item{doc_type}{The type of API documentation to generate. Can be either
\code{"rapidoc"} (the default), \code{"redoc"}, \code{"swagger"}, or \code{NULL} (equating to not
generating API docs)}

\item{doc_path}{The URL path to serve the api documentation from}

\item{doc}{A list with the OpenAPI documentation}

\item{overwrite}{Logical. Should already existing documentation be
removed or should it be merged together with \code{doc}}

\item{subset}{A character vector giving the path to the subset of the
docs to assign \code{doc} to}
}
\value{
These functions return the \code{api} object allowing for easy chaining
with the pipe
}
\description{
The \href{https://www.openapis.org}{OpenAPI standard} offers a way to describe the
various endpoints of your api in machine- and human-readable way. On top of
this, various solutions have been build to generate online documentation of
the API based on a provided OpenAPI spec. plumber2 offers support for
\href{https://rapidocweb.com}{RapiDoc}, \href{https://redocly.com/redoc}{Redoc}, and
\href{https://swagger.io}{Swagger} as a UI frontend for the documentation and will
also generate the spec for you based on the tags in parsed files. If you are
creating your API programmatically or you wish to add to the autogenerated
docs you can add docs manually, either when adding a handler (using the \code{doc}
argument), or with the \code{api_doc_add()} function
}
