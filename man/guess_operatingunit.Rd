% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hfr_dqa_utilities.R
\name{guess_operatingunit}
\alias{guess_operatingunit}
\title{Get operating unit name}
\usage{
guess_operatingunit(pfile, levels, ims)
}
\arguments{
\item{pfile}{processed file}

\item{levels}{org levels}

\item{ims}{mechanisms df}
}
\value{
operating unit name
}
\description{
Get operating unit name
}
\examples{
\dontrun{
  get_mech_ou(ims, 'HFR_2020.99_XAR_100000_processed_20200528.csv')
}

}
