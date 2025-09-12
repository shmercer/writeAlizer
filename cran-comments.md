## R CMD check results

0 errors | 0 warnings | 0 notes

## Test environments

* Local: Windows (R 4.x), `devtools::check(args = c("--as-cran"))`
* Win-builder: R-devel, R-release (both successful)
* GitHub Actions (r-lib/actions “check-standard” matrix):
  - ubuntu-latest: R-devel, R-release, R-oldrel-1
  - macos-latest: R-release
  - windows-latest: R-release
* R-hub v2: windows-latest (via GitHub Actions workflow)

## Notes for CRAN

* Examples and tests do **not** require network access. Heavy artifacts are either mocked in `tempdir()` or guarded with `\donttest{}`.
* URL checks cleaned; moved/forbidden links updated or replaced.
* `withr` is used only in tests (declared in Suggests).
* No reverse dependencies at initial submission.
