% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cc_winsleep.R
\name{cc_winsleep}
\alias{cc_winsleep}
\title{Set Windows sleep Option}
\usage{
cc_winsleep(minutes = 30)
}
\arguments{
\item{minutes}{The number of minutes after which the device should go to sleep}
}
\description{
Set number of minutes after which Windows should sleep when plugged in
}
\details{
This is a convenience function to temporarily modify / disable the number of minutes after which
when Windows goes to sleep when idle when plugged in.

This function can be run before starting a lengthy process, to prevent
windows from going to sleep when there are no mouse or keyboard interactions (i.e., 'idle').
Setting the number of minutes to '0' disables going to sleep completely.

Note that this function is i) for windows only, and ii) only sets the sleep setting that's in effect when the computer is plugged in.
}
