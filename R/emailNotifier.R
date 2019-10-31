#' Send email notifications to maintainers whose builds failed
#' @author Dinakar Kulkarni <kulkard2@gene.com>
#' @importFrom sendmailR sendmail mime_part
#' @importFrom htmlTable htmlTable
#' @importFrom stats complete.cases setNames
#' @importFrom utils read.csv write.csv
#' @param repo A gRAN repo object
#' @param mailopts Email options as a list
#' @param attachments Files with full paths, as an array
#' @return None
#' @export
#' @seealso \code{\link{getFailureInfo}} for creating failed pkg manifests,
#'          \code{\link{sendMail}} for sending emails
emailNotifier <- function (repo,
                           mailopts = email_options(repo),
                           attachments = NULL) {
  # Identify the repo_name
  repo_name <- repo@param@repo_name

  # Get email options:
  mailControl <- list(smtpServer = paste(mailopts["smtp_server"]),
                      smtpPort = paste(mailopts["smtp_port"]))
  sender <- paste(mailopts["sender_email"])

  # Create a manifest file to record changes
  manifestFile <- file.path(destination(repo),
                            paste0(".", repo_name, "failedpkgs.csv"))

  # If running for the first time, or if manifestFile has been removed
  # Get the failure info from the repo object
  if (!file.exists(manifestFile)) {
    failedPkgManifest_old <- getFailureInfo(repo)
    failedPkgManifest_old <- failedPkgManifest_old[0,]
  } else {
    failedPkgManifest_old <-
      as.data.frame(read.csv(manifestFile, header = TRUE))
  }

  failedPkgManifest_new <- getFailureInfo(repo)

  # Find the differences/delta between the new and old manifests
  cnames <- c("name",
              "version",
              "lastAttemptStatus",
              "lastAttemptVersion",
              "email")
  failedPkgManifest_old <- failedPkgManifest_old[, cnames]
  failedPkgManifest_new <- failedPkgManifest_new[, cnames]
  write.csv(failedPkgManifest_old, file = manifestFile)
  notifiablePkgs <-
    tryCatch(
      deltaDF(failedPkgManifest_new, failedPkgManifest_old),
      error = function(e) {
        setNames(data.frame(matrix(
          ncol = length(cnames), nrow = 0
        )), cnames)
      }
    )

  # Send the notifications
  notifyManifest(notifiablePkgs,
                 repo,
                 mailopts = mailopts,
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
  trimws(toString(sort(unlist(
    strsplit(string, split = delimiter)
  ), ...)))
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

  cnames <- c(
    "name",
    "version",
    "lastAttempt",
    "lastAttemptStatus",
    "lastAttemptVersion",
    "lastbuilt",
    "lastbuiltversion",
    "lastbuiltstatus",
    "maintainer"
  )

  RepoResultsDF <- repo_results(repo)[, cnames]
  RepoResultsDF <-
    RepoResultsDF[!RepoResultsDF$name %in% suspended_pkgs(repo),]
  failedPackages <-
    RepoResultsDF[grep("fail", RepoResultsDF$lastAttemptStatus), ]
  failedPackages <-
    failedPackages[complete.cases(failedPackages[, "maintainer"]), ]
  failedPackages$email <- regmatches(
    failedPackages$maintainer,
    gregexpr("(?<=\\<).*?(?=\\>)",
             failedPackages$maintainer, perl = T)
  )
  failedPackages$email <- as.character(failedPackages$email)
  failedPackages$email <- gsub("c\\(\"", "", failedPackages$email)
  failedPackages$email <- gsub("\", \"", ",", failedPackages$email)
  failedPackages$email <- gsub("\"\\)", "", failedPackages$email)

  for (i in 1:NROW(failedPackages)) {
    failedPackages[i, "email"] = sortDelimitedString(failedPackages[i, "email"])
  }

  failedPackages <-
    failedPackages[,!(colnames(failedPackages) %in% c("maintainer"))]
  rownames(failedPackages) <- NULL

  return(failedPackages)
}


#' Wrapper for sendmail, allows for multiple attachments to be sent
#' @param receiver Receiver's email ID as a string. Vector if multiple email IDs
#' @param subject Email subject as a string
#' @param body Email message body as a string
#' @param repo A gRAN repo object
#' @param mailopts Email options as a list
#' @param attachments Files with full paths as an array
#' @return None
#' @seealso \code{\link[sendmailR]{sendmail}} for sending simple emails
#' @note This function is not intended for direct use by the end user.
sendMail <- function(receiver,
                     subject,
                     body,
                     repo,
                     mailopts = email_options(repo),
                     attachments = NULL) {
  if (missing(receiver) ||
      missing(subject) || missing(body) || missing(repo)) {
    stop(paste("Missing mandatory arguments to", match.call()[[1]]))
  }

  # Get email options:
  mailControl <- list(smtpServer = paste(mailopts["smtp_server"]),
                      smtpPort = paste(mailopts["smtp_port"]))
  sender <- paste(mailopts["sender_email"])

  body <- mime_part(body)
  body[["headers"]][["Content-Type"]] <- "text/html"
  bodyWithAttachment <- list(body)

  for (attachmentPath in attachments) {
    if (!file.exists(attachmentPath)) {
      stop(paste(
        "Halting execution. Please check that the file",
        attachmentPath,
        "exists"
      ))
    }
    attachmentName <- basename(attachmentPath)
    attachmentObject <-
      mime_part(x = attachmentPath, name = attachmentName)
    bodyWithAttachment[[length(bodyWithAttachment) + 1]] <-
      attachmentObject
  }
  sendmailV <- Vectorize(sendmail , vectorize.args = "to")
  receiver <- unlist(strsplit(receiver, split = ","))
  status <-
    sendmailV(
      from = sender,
      to = receiver,
      subject = subject,
      msg = bodyWithAttachment,
      control = mailControl
    )

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
  unsubscribe_list <- email_options(repo)["unsubscribers"]
  for (emailID in unique(manifest$email)) {
    if (isValidEmail(emailID)) {
      subDF <- manifest[manifest[, "email"] == emailID,]
      emailSubject <-
        paste0("Packages that failed to build on ", platform(repo), " GRAN",
               repo@param@repo_name,
               ": ",
               Sys.time())
      emailBody <-
        htmlTable(
          subDF[, c("name", "version", "lastAttemptStatus")],
          caption = "The following packages failed to build in GRAN:",
          css.cell = ("padding-left: 1em; padding-right: 1em"),
          rnames = rep("", nrow(subDF)),
          tfoot = paste(
            "<br>To fix this, please checkin",
            "fixes to the packages including a version bump, ",
            "and they'll be rebuilt the following night.<br>",
            "Log info available here:",
            buildReportURL(repo)
          )
        )
      is_unsubscriber <-
        grepl(paste(unsubscribe_list, collapse = "|"), emailID,
              perl = TRUE)
      if (!is_unsubscriber) {
        sendMail(emailID, emailSubject, emailBody, repo, ...)
      }
    }
  }
}


#' Constructs the gRAN build report URL
#' @param repo A gRAN repo object
#' @note This function is not intended for direct use by the end user.
buildReportURL <- function(repo) {
  base_url <- param(repo)@dest_url
  sub_url <- gsub("^.*\\//", "", destination(repo))
  paste0(base_url, "/", sub_url, "/buildreport.html")
}
