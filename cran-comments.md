## writeAlizer 1.6.5

- Addresses CRAN policy for Internet resources:
  - Network access now fails gracefully with informative messages (no low-level `download.file` errors).
  - Tests that rely on remote artifacts now preflight their exact URLs/paths and skip when unavailable.
  - Examples/tests seed mock artifacts for deterministic offline runs.

R CMD check --as-cran is clean locally on Windows with and without internet.
