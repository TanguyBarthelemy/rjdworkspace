% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/copy_ws.R
\name{copy_ws}
\alias{copy_ws}
\title{Copy a WS}
\usage{
copy_ws(ws_name, from, to = tempdir(), overwrite = TRUE, verbose = TRUE)
}
\arguments{
\item{ws_name}{the name of the WS}

\item{from}{the path to the folder containing the WS (the XML file + the WS
folder)}

\item{to}{the path to the new folder which will contains the WS (the XML
file + the WS folder)}

\item{overwrite}{Overwrite existing file (Defaults to TRUE)}

\item{verbose}{A boolean to print indications on the processing status
(optional and TRUE by default)}
}
\value{
the function returns invisibly (with \code{invisible()}) a boolean
specifying if the transfer was done or an error if the specified paths or
workspace don't exists
}
\description{
Copy a WS
}
\examples{
# Déplacement d'un WS dans un environnement temporaire
destination_dir <- tempdir()

# Copy of a worspace in a temporary environment
copy_ws(
  ws_name = "ws_output",
  from = file.path(system.file("extdata", package = "rjdworkspace"), "WS"),
  to = destination_dir
)

}
