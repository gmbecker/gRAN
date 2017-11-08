Changes in version 1.6.5 (2017-10-30)

- New Features
  * Parallelize creation of covr reports
  * Create a CRAN-style archive directory
  * Performance improvements

- Bugfixes
  * Don't create covr reports if check_test is off
  * Handle cases when all packages build successfully

Changes in version 1.6.4 (2017-09-01)

- New Features
  * Default to archiving packages when repo is cleared
  * Do not display suspended packages in the build report
  * Package JSON has a "GranRepo" field

- Bugfixes
  * Do not build splash pages for packages that are not successfully building
  * Get rid of spurious warnings
  * Ensure correct R is invoked when building pkgs.
  * Retain first line of log in the output logs instead of replacing it

Changes in version 1.6.3 (2017-08-26)

- New Features
  * hexStickers for packages in splash pages
  * Email preferences, settings and options. Unsubscribe list, specify SMTP server options

- Bugfixes
  * Build reports, splash pages have margins and padding

Changes in version 1.6.0 (2017-08-02)

- New Features
  * Risk reports, build badges for covr reports & build status
  * Beautified build report

Changes in version 1.5.0 (2017-07-18)

- New Features
  * Splash pages for GRAN packages
  * Build report now shows install logs

Changes in version 1.4.0 (2017-07-12)

- New Features
  * Add test coverage report statistics and render HTML reports for the same

- Bugfixes
  * The "suspended" field in the repo results now shows the correct value
  * Updated criteria to identify check fails
  * Create missing directories if needed

Changes in version 1.3.3 (2017-06-12)

- Bugfixes
  * Locate DESCRIPTION file inside the subdirectory of the package downloaded from any Git source

- New Features
  * Email notifications are sent to package authors whose builds failed

Changes in version 1.2.1 (2017-02-09)

- API Changes
  * Add escape valve which immediately returns NULL from makeRepo if
    git is not available.

Changes in version 1.1.12 (2016-05-17)

- Bugfixes
  * Fix bug in loadRepo that called prepDirStructure with the wrong basedir and destination

Changes in version 1.1.10 (2016-05-17)

- API Changes
  * add support for single-package logging

Changes in version 1.1.5 (2016-3-23)

- Bugfixes
  * Don't build or test installation of GRAN<reponame> package, as this can break when they have versioned dependences on GRANBase, which is loaded at the time.

Changes in version 1.1.4 (2016-02-10)

- Bugfixes
  * Make sure that repos for install test are forced *before* changing libpath!!!

Changes in version 1.1.3 (2016-02-09)

- Bugfixes
  * Fix bug in installTest that was using .libPaths instead of .libPaths2, causing the site library to be present and mess up dependency installation, leading to erroneous build failures.
  * require switchr 0.9.26 for upstread updateSVN bugfix

Changes in version 1.1.2 (2015-11-10)

- Bugfixes
  * Fix bug in using new system_w_init (env expects charvec of "VAR=value", NOT named char vec.

- API changes
  * require switchr version 0.9.22

Changes in version 1.1.1 (2015-09-29)

- Bugfixes
  * Fix occasional infinite recursion error when writing build report.

- API changes
  * Use hwriter to write out build report. Remove dependency on XML package.

Changes in version 1.0.26 (2015-09-18)

- Bugfixes
  * Handle missing installation output (seemingly) caused by a dependency failing to install during parallel installation.

Changes in version 1.0.25 (2015-09-17)

- API changes
  * Installation of packages during package test now (again) occurs in parallel

Changes in version 1.0.18 (2015-07-24)

- Bugfixes
  * change in way tarballs are detected during check test to avoid
    missing tarball bug

Changes in version 1.0.15 (2015-07-16)

- API changes
  * clear_repo and clear_temp_fils now return the GRANRepository object
  * clear_repo now resets the repo build results

Changes in version 1.0.14 (2015-07-15)
- Bugfixes
  * Include clear.R in github repo


Changes in version 1.0.13 (2015-07-14)
- Bugfixes
  * No longer erroneously clear temp repo when install and check testing is disabled

Changes in version 1.0.12 (2015-07-01)
- Bugfixes
  * Now import symbols from utils explicitly.

Changes in version 1.0.11 (2015-06-30)

- Bugfixes
  * Fix code in repo-specific GRAN package generated during build process.

- New Features
  * added clear_repo and clear_temp_fils convenience function for clearing out files in order to build a fresh repo.


Changes in version 1.0.10 (2015-06-29)

- Bugfixes
  * Remove unintentional logging to ~/granchecklog file
  * Provide default values for archive_timing and archive_retries in RepoBuildParam constructor
  * Temporarily disable use_cran_granbase functionality

Changes in version 1.0.9 (2015-06-25)

- Bugfixes
  * Safer rbind in GRANRepository addPkg method
  * Safer file-url handling using utilities from switchr
