% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/emailNotifier.R
\name{sendMail}
\alias{sendMail}
\title{Wrapper for sendmail, allows for multiple attachments to be sent}
\usage{
sendMail(receiver, subject, body, repo, mailopts = email_options(repo),
  attachments = NULL)
}
\arguments{
\item{receiver}{Receiver's email ID as a string. Vector if multiple email IDs}

\item{subject}{Email subject as a string}

\item{body}{Email message body as a string}

\item{repo}{A gRAN repo object}

\item{mailopts}{Email options as a list}

\item{attachments}{Files with full paths as an array}
}
\value{
None
}
\description{
Wrapper for sendmail, allows for multiple attachments to be sent
}
\note{
This function is not intended for direct use by the end user.
}
\seealso{
\code{\link[sendmailR]{sendmail}} for sending simple emails
}
