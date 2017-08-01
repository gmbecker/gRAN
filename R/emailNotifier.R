#' Send email notifications to maintainers whose builds failed
#' @importFrom sendmailR sendmail mime_part
#' @importFrom htmlTable htmlTable
#' @importFrom dplyr anti_join
#' @importFrom stats complete.cases setNames
#' @importFrom utils read.csv write.csv
#' @param repo A gRAN repo object
#' @param mailControl List object containing SMTP server info
#' @param sender Sender's email ID as a string
#' @param attachments Files with full paths, as an array
#' @return None
#' @export
#' @seealso \code{\link{getFailureInfo}} for creating failed pkg manifests,
#'          \code{\link{sendMail}} for sending emails,
#'          \code{\link{deltaDF}} for difference between 2 dataframes,
#'          \code{\link{notifyManifest}} for manifest-based emails
emailNotifier <- function (repo,
                           mailControl = list(smtpServer = "localhost"),
                           sender = paste0("resgran-d@",
                                          system('hostname -d', intern = TRUE)),
                           attachments = NULL) {
  # Identify the repo_name
  repo_name <- repo@param@repo_name

  # Create a manifest file to record changes
  manifestFile <- paste0(".", repo_name, "failedpkgs.csv")

  # If running for the first time, or if manifestFile has been removed
  # Get the failure info from the repo object
  if (!file.exists(manifestFile)) {
    failedPkgManifest_old <- getFailureInfo(repo)
    failedPkgManifest_old <- failedPkgManifest_old[0, ]
  } else {
    failedPkgManifest_old <- as.data.frame(read.csv(manifestFile, header=TRUE))
  }

  failedPkgManifest_new <- getFailureInfo(repo)

  # Find the differences/delta between the new and old manifests
  cnames <- c("name", "version", "lastAttemptStatus",
              "lastAttemptVersion", "email")
  failedPkgManifest_old <- failedPkgManifest_old[, cnames]
  failedPkgManifest_new <- failedPkgManifest_new[, cnames]
  write.csv(failedPkgManifest_old, file = manifestFile)
  notifiablePkgs <- tryCatch(deltaDF(failedPkgManifest_new, failedPkgManifest_old),
                    error = function(e) {
                      setNames(data.frame(matrix(ncol = length(cnames), nrow = 0)), cnames)
                    })

  # Send the notifications
  notifyManifest(notifiablePkgs,
                 repo,
                 mailControl = mailControl,
                 sender = sender,
                 attachments = attachments)

  # Write the new failure manifest to the manifest file
  write.csv(failedPkgManifest_new, file = manifestFile)
}


#' Alphabetically sort delimited strings
#' @param string A string
#' @param delimiter A delimiter that separates contents of string
#' @param ... Arguments that you want to pass to base::sort()
#' @return String that is alphabetically sorted
#' @note This function is not intended for direct use by the end user.
sortDelimitedString <- function(string, delimiter = ",", ...) {
  trimws(toString(sort(unlist(strsplit(string, split=delimiter)), ...)))
}


#' Creates a dataframe containing information regarding packages that had a
#' failed status
#' @param repo A GRAN repo object
#' @return Dataframe containing package info that failed
#' @seealso \code{\link{repo_results}} for repo results as a dataframe
#' @note This function is not intended for direct use by the end user.
getFailureInfo <- function(repo) {
  if (missing(repo)) {
    stop(paste("Missing arguments to", match.call()[[1]]))
  }

  cnames <- c("name", "version", "lastAttempt", "lastAttemptStatus",
              "lastAttemptVersion", "lastbuilt", "lastbuiltversion",
              "lastbuiltstatus", "maintainer")

  RepoResultsDF <- repo_results(repo)[, cnames]
  RepoResultsDF <- RepoResultsDF[! RepoResultsDF$name %in% suspended_pkgs(repo), ]
  failedPackages <- RepoResultsDF[grep("fail", RepoResultsDF$lastAttemptStatus),]
  failedPackages <- failedPackages[complete.cases(failedPackages[, "maintainer"]),]
  failedPackages$email <- regmatches(failedPackages$maintainer,
                                     gregexpr("(?<=\\<).*?(?=\\>)",
                                     failedPackages$maintainer, perl = T))
  failedPackages$email <- as.character(failedPackages$email)
  failedPackages$email <- gsub("c\\(\"", "", failedPackages$email)
  failedPackages$email <- gsub("\", \"", ",", failedPackages$email )
  failedPackages$email <- gsub("\"\\)", "", failedPackages$email)

  for (i in 1:NROW(failedPackages)) {
    failedPackages[i, "email"] = sortDelimitedString(failedPackages[i, "email"])
  }

  failedPackages <- failedPackages[, !(colnames(failedPackages) %in% c("maintainer"))]
  rownames(failedPackages) <- NULL

  return(failedPackages)
}


#' Wrapper for sendmail, allows for multiple attachments to be sent
#' @param receiver Receiver's email ID as a string. Vector if multiple email IDs
#' @param subject Email subject as a string
#' @param body Email message body as a string
#' @param mailControl List object containing SMTP server info
#' @param sender Sender's email ID as a string
#' @param attachments Files with full paths as an array
#' @return None
#' @seealso \code{\link[sendmailR]{sendmail}} for sending simple emails
#' @note This function is not intended for direct use by the end user.
sendMail <- function(receiver,
                     subject,
                     body,
                     mailControl = list(smtpServer = "localhost"),
                     sender = paste0("resgran-d@", system('hostname -d')),
                     attachments = NULL) {
  if (missing(receiver) || missing(subject) || missing(body)) {
    stop(paste("Missing mandatory arguments to", match.call()[[1]]))
  }
  body <- mime_part(body)
  body[["headers"]][["Content-Type"]] <- "text/html"
  bodyWithAttachment <- list(body)

  for (attachmentPath in attachments) {
    if (!file.exists(attachmentPath)) {
      stop(paste("Halting execution. Please check that the file",
                 attachmentPath, "exists"))
    }
    attachmentName <- basename(attachmentPath)
    attachmentObject <- mime_part(x = attachmentPath, name = attachmentName)
    bodyWithAttachment[[length(bodyWithAttachment) + 1]] <- attachmentObject
  }
  sendmailV <- Vectorize(sendmail , vectorize.args = "to")
  receiver <- unlist(strsplit(receiver, split=","))
  status <- sendmailV(from = sender, to = receiver, subject = subject,
                     msg = bodyWithAttachment, control = mailControl)

  if (status[[1]] != 221) {
    stop(paste("Failed to send email to", receiver))
  }
}


#' Sends email notifications for a given manifest
#' @param manifest A dataframe obtained from getFailureInfo
#' @param repo A gRAN repo object
#' @param ... Additonal arguments to GRANBase::sendMail()
#' @return None
#' @seealso \code{\link{sendMail}} for sending emails,
#'          \code{\link{isValidEmail}} for validating email,
#'          \code{\link[htmlTable]{htmlTable}} for creating HTML tables
#' @note This function is not intended for direct use by the end user.
notifyManifest <- function(manifest, repo, ...) {
  if (missing(manifest) || missing(repo)) {
    stop(paste("Missing mandatory arguments to", match.call()[[1]]))
  }
  for (emailID in unique(manifest$email)) {
    if (isValidEmail(emailID)) {
      subDF <- manifest[manifest[, "email"] == emailID, ]
      emailSubject <- "GRAN packages that failed to build"
      emailBody <- htmlTable(subDF[, c("name", "version", "lastAttemptStatus")],
                             caption="The following packages failed to build in GRAN:",
                             css.cell = ("padding-left: 1em; padding-left: 1em"),
                             rnames = rep("",nrow(subDF)),
                             tfoot=paste("<br>To fix this, please checkin",
                             "fixes to the packages including a version bump, ",
                             "and they'll be rebuilt the following night.<br>",
                             "Log info available here:", buildReportURL(repo)))
      gnesys <- system('hostname -d', intern = TRUE)
      if(gnesys == "gene.com") {
        domain <- substrRight(emailID, 9)
        if(domain == "@gene.com" || domain == "roche.com") {
          sendMail(emailID, emailSubject, emailBody, ...)
        }
      } else sendMail(emailID, emailSubject, emailBody, ...)
    }
  }
}


#' Constructs the gRAN build report URL
#' @param repo A gRAN repo object
#' @note This function is not intended for direct use by the end user.
buildReportURL <- function(repo) {
  base_url <- param(repo)@dest_url
  sub_url <- gsub("^.*\\//","", destination(repo))
  paste0(base_url, "/", sub_url, "/buildreport.html")
}


#' Returns the difference between 2 data frames
#' @param new_df The new dataframe which you want to compare
#' @param old_df An older dataframe of the same structure
#' @return Differences as a dataframe of the same structure
#' @seealso \code{\link[dplyr]{anti_join}}
#' @note This function is not intended for direct use by the end user.
deltaDF <- function(new_df, old_df) {
  delta <- suppressMessages(anti_join(new_df, old_df))
  return(delta)
}


#' Checks whether an email ID is valid
#' @param email_id Email ID as a string
#' @return Boolean
#' @note This function is not intended for direct use by the end user.
isValidEmail <- function(email_id) {
	grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>",
        as.character(email_id), ignore.case=TRUE)
}
