## Resubmission

Thanks for reviewing the submission of the `forcis` package. Here is the answer to your requested changes:

> _Please omit the redundant "An R" at the start of your title._

* Done. The title is now "Handle the FORCIS Foraminifera Database" ([#7dafab7](https://github.com/ropensci/forcis/pull/131/commits/7dafab7af1b15636b8237ab3e4dd53c8bc9478a8)).


> _Please always write package names, software names and API names in single quotes in title and description._

* Done. I used single quotes to mention the 'FORCIS' database in the description field ([#9bde0fd](https://github.com/ropensci/forcis/pull/131/commits/9bde0fde9779cf90f18b9e9d01385420fb266c6f)).

> _Please write references in the description of the DESCRIPTION file in the form authors (year) \<doi:...\>_

* Done. I added author and (year) for the Zenodo reference ([#2009c6b](https://github.com/ropensci/forcis/pull/131/commits/2009c6beb1b4074661269a73e0daf8957d68a500)).

> _Please unwrap the examples if they are executable in < 5 sec, or replace dontrun{} with \donttest{}._

* Done. I replaced `\dontrun{}` by `\donttest{}` in functions making request to the Zenodo API because the execution time can be > 5s ([#8caefb8](https://github.com/ropensci/forcis/pull/131/commits/8caefb8fe0a42945346491ad5b293cab3a055e58)).

> _Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd())._

* Done. I removed the default value (current directory) of the argument `path` in `download_forcis_db()` and `read_*_data()` functions ([#39be485](https://github.com/ropensci/forcis/pull/131/commits/39be485e5c22a2e92386649f668a3d14bb4b7ca9)) and used the function `tempdir()` in examples and tests ([#f4a8e4a](https://github.com/ropensci/forcis/pull/131/commits/f4a8e4a2899373f3bba6386963b8dbe34f4f31a8)).

Thanks for your review.



## Test environments

* Local
  * Arch Linux 6.14.6-arch1-1 install, R 4.5.0
  
* GitHub Actions
  * macOS 14.7.5 23H527, R-release (R 4.5.0)
  * Windows Server 2022 10.0.20348, R-release (R 4.5.0)
  * Ubuntu 24.04.2 LTS, R-devel, R-release (R 4.5.0), R-oldrel
  
* WinBuilder
  * R-devel
  * R-release
  * R-oldrel



## R CMD check results

0 errors | 0 warnings | 1 note

* New submission
* Possibly misspelled words in DESCRIPTION (not appropriate)



## Downstream dependencies

There are currently no downstream dependencies for this package.
