% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hfr_dqa_utilities.R
\name{update_operatingunits}
\alias{update_operatingunits}
\title{Update invalid operating units}
\usage{
update_operatingunits(hfr_data, levels, orgs, ims = NULL)
}
\arguments{
\item{hfr_data}{processed hfr data}

\item{levels}{datim org levels}

\item{orgs}{datim org hierarchy}

\item{ims}{datim mechanisms}
}
\value{
hfr_data df
}
\description{
Update invalid operating units
}
\examples{
\dontrun{
  update_operatingunits(hfr_df, levels=org_levels, orgs=org_hierarchy, ims=mechanisms)
}
}
