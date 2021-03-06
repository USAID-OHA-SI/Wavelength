% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_mer.R
\name{pull_mer}
\alias{pull_mer}
\title{Extract DATIM Results and Targets (DATIM API Call)}
\usage{
pull_mer(
  ou_name = NULL,
  username,
  password,
  baseurl = "https://final.datim.org/",
  quarters_complete = NULL,
  folderpath_output = NULL
)
}
\arguments{
\item{ou_name}{Operating Unit name, if mechanism is not specified}

\item{username}{DATIM username}

\item{password}{DATIM password, recommend using \code{mypwd()}}

\item{baseurl}{API base url, default = https://final.datim.org/}

\item{quarters_complete}{no. of quarters completed through FY to determine weeks left in year}

\item{folderpath_output}{folder path to store DATIM output, default = NULL}
}
\description{
Extract DATIM Results and Targets (DATIM API Call)
}
\examples{
\dontrun{
 #ou targets
 myuser <- "UserX"
 mech_x_targets <- pull_mer(ou_name = "Namibia", username = myuser, password = mypwd(myuser))
 }
}
