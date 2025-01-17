% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_price_data.R
\name{read_price_data}
\alias{read_price_data}
\title{Read in price data from csv and check that all expected columns are given.}
\usage{
read_price_data(path, version, expected_technologies)
}
\arguments{
\item{path}{A string that points to the location of the file containing the
price data}

\item{version}{A string indicating whether to prepare the capacity factors
based on the old price data structure or the new long format. Must be
either "old" or "new".}

\item{expected_technologies}{A character vector listing all technologies for
which price data must be provided}
}
\description{
Read in price data from csv and check that all expected columns are given.
}
\seealso{
Other import functions: 
\code{\link{generate_transition_shocks}()},
\code{\link{read_capacity_factors}()},
\code{\link{read_financial_data}()},
\code{\link{read_ngfs_carbon_tax}()},
\code{\link{read_pacta_results}()},
\code{\link{read_transition_scenarios}()}
}
\concept{import functions}
