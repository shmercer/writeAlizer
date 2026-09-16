<img src="https://www.dropbox.com/s/bxgse42uf44k6me/wA_logo.png?raw=1" alt="writeAlizer logo" width="240" height="105">

# writeAlizer: An R Package to Generate Automated Writing Quality Scores

<!-- badges: start -->
  [![CRAN status](https://www.r-pkg.org/badges/version/writeAlizer)](https://CRAN.R-project.org/package=writeAlizer)
  [![R-CMD-check.yaml](https://github.com/shmercer/writeAlizer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shmercer/writeAlizer/actions/workflows/R-CMD-check.yaml)
  [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://github.com/shmercer/writeAlizer/blob/master/LICENSE.md)
  [![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
  [![Codecov test coverage](https://codecov.io/gh/shmercer/writeAlizer/graph/badge.svg)](https://app.codecov.io/gh/shmercer/writeAlizer)
<!-- badges: end -->

writeAlizer turns output from text analysis programs into research-based estimates of writing quality or written-expression curriculum-based measurement (CBM) scores. It imports your analysis files, downloads the scoring models the first time they are needed, and returns a table of scores matched to your text IDs.

## Start here

Choose the row that matches the program you used to analyze your writing samples:

| Your analysis file | What you can score | Model to start with |
|---|---|---|
| ReaderBench Java CSV | Overall writing quality | `rb_mod3all` |
| Coh-Metrix 3.0 CSV | Overall writing quality | `coh_mod3all` |
| GAMET 1.0 CSV | Word counts, spelling, and word sequences | `gamet_cws1` |

New to these programs? The [getting-started guide](https://shmercer.github.io/writeAlizer/articles/writealizer-getting-started.html) walks through preparing your files and using each program, with screenshots. writeAlizer reads their CSV output; it does not analyze raw essays directly.

### 1. Install and load writeAlizer

Run these commands in the R console:

```r
install.packages("writeAlizer")  # Install once
library(writeAlizer)            # Load at the start of each R session
```

Some scoring models need additional R packages. This command lists any that are missing and prints an installation command you can copy:

```r
model_deps()
```

`model_deps()` reports all packages in the package's optional dependency list (`Suggests`), including documentation and testing tools. It does not install anything. Its `required` result lists those packages; `missing` lists the ones that are not installed. It checks availability, not version requirements.

### 2. Try a sample file

This example uses a small ReaderBench CSV included with writeAlizer, so you do not need to prepare your own data yet. **The first scoring run needs an internet connection** to download model files.

```r
rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
rb <- import_rb(rb_path)
quality <- predict_quality("rb_mod3all", rb)

# Show each text's ID and overall predicted writing quality
quality[c("ID", "pred_rb_mod3all_mean")]
```

For a demonstration without downloads, see the guide's [offline example](https://shmercer.github.io/writeAlizer/articles/writealizer-getting-started.html#offline-example). That example demonstrates the workflow; its constant scores are not writing assessments.

### 3. Score your own files

Replace the example path below with the location of your CSV. Forward slashes work in R on Windows as well as macOS and Linux.

```r
rb <- import_rb("C:/Users/YourName/Documents/ReaderBench_output.csv")
quality <- predict_quality("rb_mod3all", rb)
write.csv(quality, "writing_scores.csv", row.names = FALSE)
```

For Coh-Metrix, use `import_coh()` with `"coh_mod3all"`. For GAMET, use `import_gamet()` with `"gamet_cws1"`. The [guide](https://shmercer.github.io/writeAlizer/articles/writealizer-getting-started.html#importing-data) includes examples for all three.

Keep the original column names from your analysis program. Each row must have a unique, nonblank text ID. Imports preserve IDs as text, including leading zeros, and sort rows by ID. Coh-Metrix and GAMET imports remove directory paths and a trailing `.txt` extension; ReaderBench keeps the `File.name` value as its ID.

### Understanding your results

- **ReaderBench and Coh-Metrix:** the recommended all-genre models return three genre-specific predictions and an overall mean (`pred_rb_mod3all_mean` or `pred_coh_mod3all_mean`). Single-genre models return one prediction without a mean column.
- **GAMET:** results contain Total Words Written (TWW), Words Spelled Correctly (WSC), Correct Word Sequences (CWS), and Correct Minus Incorrect Word Sequences (CIWS). See the [output guide](https://shmercer.github.io/writeAlizer/articles/writealizer-getting-started.html#predicting-writing-quality) for exact column names.
- **IDs:** every result retains the text ID so you can match it to the original writing sample.

For ReaderBench and Coh-Metrix Models 2 and 3, predictors are standardized using the group of texts you submit in that call. Changing that group can change a text's score. Use a consistent scoring group for comparisons; a single text or a feature with no variation can produce missing values. These are model-based estimates, not percentages or universal proficiency cutoffs. The [model-development guide](https://shmercer.github.io/writeAlizer/articles/scoring-model-development.html) explains the research behind them.

## Model downloads and offline use

Downloaded model files are saved in a cache: a folder writeAlizer reuses on later runs.

```r
wa_cache_dir()  # Show the cache location
```

After all files for a model have been downloaded, you can use that model offline. To prevent new internet downloads explicitly, set `options(writeAlizer.offline = TRUE)`; set it back to `FALSE` when you want downloads again.

If you need to remove downloaded models, use `wa_cache_clear()`. In an interactive R session it shows a preview and asks before deleting. In a script it clears without prompting. The next scoring run will need to download those models again. If you set a custom cache location with `options(writeAlizer.cache_dir = "path/to/cache")`, use a dedicated folder: clearing the cache removes everything in it.

## More help

- [Getting started and troubleshooting](https://shmercer.github.io/writeAlizer/articles/writealizer-getting-started.html)
- [Function reference](https://shmercer.github.io/writeAlizer/reference/index.html), or `help("predict_quality")` in R
- [Scoring model development and research](https://shmercer.github.io/writeAlizer/articles/scoring-model-development.html)
- [Version history](https://github.com/shmercer/writeAlizer/blob/master/NEWS.md)

### Development version

Most users should install from CRAN as shown above. To try the development version from GitHub:

```r
# install.packages("pak")  # If needed
pak::pak("shmercer/writeAlizer")
```

## Package Author and Maintainer

* **Sterett H. Mercer** - *University of British Columbia*  
UBC Faculty Profile: https://ecps.educ.ubc.ca/sterett-h-mercer/  
ResearchGate: https://www.researchgate.net/profile/Sterett_Mercer  
Google Scholar: https://scholar.google.ca/citations?user=YJg4svsAAAAJ&hl=en  

Also see the list of code [contributors](https://github.com/shmercer/writeAlizer/contributors) for this package.

## References

### Journal Articles

Matta, M., Keller-Margulis, M. A., & Mercer, S. H. (2025). Improving written-expression curriculum-based measurement feasibility with automated writing evaluation programs. *School Psychology, 40*(6), 707–717. https://doi.org/10.1037/spq0000691

Matta, M., Mercer, S. H., & Keller-Margulis, M. A. (2023). Implications of bias in automated writing quality scores for fair and equitable assessment decisions. *School Psychology, 38*, 173–181. https://doi.org/10.1037/spq0000517

Matta, M., Mercer, S. H., & Keller-Margulis, M. A. (2022). Evaluating validity and bias for hand-calculated and automated written expression curriculum-based measurement scores. *Assessment in Education: Principles, Policy & Practice, 29*, 200-218. https://doi.org/10.1080/0969594X.2022.2043240

Mercer, S. H., & Cannon, J. E. (2022). Validity of automated learning progress assessment in English written expression for students with learning difficulties. *Journal for Educational Research Online, 14*, 39-60. https://doi.org/10.31244/jero.2022.01.03

Matta, M., Keller-Margulis, M. A., & Mercer, S. H. (2022). Cost analysis and cost effectiveness of hand-scored and automated approaches to writing screening. *Journal of School Psychology, 92*, 80-95. https://doi.org/10.1016/j.jsp.2022.03.003

Keller-Margulis, M. A., Mercer, S. H., & Matta, M. (2021). Validity of automated text evaluation tools for written-expression curriculum-based measurement: A comparison study. *Reading and Writing: An Interdisciplinary Journal, 34*, 2461-2480. https://doi.org/10.1007/s11145-021-10153-6  

Mercer, S. H., Cannon, J. E., Squires, B., Guo, Y., & Pinco, E. (2021). Accuracy of automated written expression curriculum-based measurement scoring. *Canadian Journal of School Psychology, 36*, 304-317. https://doi.org/10.1177/0829573520987753

Mercer, S. H., Keller-Margulis, M. A., Faith, E. L., Reid, E. K., & Ochs, S. (2019). The potential for automated text evaluation to improve the technical adequacy of written expression curriculum-based measurement. *Learning Disability Quarterly, 42*, 117-128. https://doi.org/10.1177/0731948718803296

### Conference Presentations

Keller-Margulis, M. A., Mercer, S. H., Matta, M., Hut, A. R., Navarro, S., & Duran, B. J. (2025, February). *Cross-genre validity of automated scoring of writing CBM.* Poster presented at the meeting of the National Association of School Psychologists, Seattle, WA, USA.

Keller-Margulis, M., Mercer, S. H., Matta, M., Duran, B., Hut, A., Jellinek-Russo, E., & Lozano, I. (2024, February). *Updated validity of automated scoring for writing CBM across genres.* Paper presented at the meeting of the National Association of School Psychologists, New Orleans, LA, USA.

Keller-Margulis, M. A., Mercer, S. H., Matta, M., Duran, B. J., Hut, A. R., Jellinek, E. R., Loria, E. S., & Lozano, I. (2023, February). *Validity of automated scoring of written expression CBM across genres.* Paper presented at the meeting of the National Association of School Psychologists, Denver, CO, USA.

Mercer, S. H.,Geres-Smith, R., Guo, Y., & Squires, B. (2023, February). *Validity of automated learning progress assessment in written expression.* Poster presented at the meeting of the National Association of School Psychologists, Denver, CO, USA. [https://doi.org/10.17605/OSF.IO/WHJD3](https://doi.org/10.17605/OSF.IO/WHJD3)

Matta, M., Keller-Margulis M., & Mercer, S. H. (2022, February). *New directions for writing assessment: Improving feasibility with automated scoring.* Presentation at the meeting of the National Association of School Psychologists, Boston, MA, USA.

Matta, M., Keller-Margulis, M., & Mercer, S. H. (2021, July). *The use of automated approaches to scoring written expression of elementary students.* Poster presented at the at the meeting of the International School Psychology Association, online.

Matta, Michael, Keller-Margulis, M. A., Mercer, S. H., & Zopatti, K. (2021, February). *Improving written-expression curriculum-based measurement feasibility with automated text evaluation programs.* Paper presented at the meeting of the National Association of School Psychologists, online.

Mercer, S. H., Keller-Margulis, M. A., & Matta, M. (2020, February). _[Validity of automated vs. hand-scored written expression curriculum-based measurement samples](https://blogs.ubc.ca/mercer/2020/02/11/pcrc-2020-poster-automated-text-eval-for-screening/)._ Poster presented at the Pacific Coast Research Conference, Coronado, CA, USA.

Mercer, S. H., & Cannon, J. E. (2020, February). _[Monitoring the written expression gains of learners during intensive writing intervention](https://blogs.ubc.ca/mercer/2020/02/11/pcrc-2020-automated-text-eval-for-progress-monitoring/)._ Poster presented at the Pacific Coast Research Conference, Coronado, CA, USA.

Keller-Margulis, M. A., & Mercer, S. H. (2019, August). _[Validity of automated scoring for written expression curriculum-based measurement](https://blogs.ubc.ca/mercer/2019/12/18/ies-pi-meeting-2020/)._ Poster presented at the meeting of the American Psychological Association, Chicago, IL, USA.

Mercer, S. H., Tsiriotakis, I., Kwon, E., & Cannon, J. E. (2019, June). _[Evaluating elementary students' response to intervention in written expression](https://blogs.ubc.ca/mercer/2019/06/01/csse-2019-presentation-paper-and-slides/)._ Paper presented at the meeting of the Canadian Association for Educational Psychology (Canadian Society of the Study of Education), Vancouver, BC, Canada.

## License

This project is licensed under the MIT License. See [License](https://github.com/shmercer/writeAlizer/blob/master/LICENSE.md) for details.

## Acknowledgments

 * The research reported here was supported by the Institute of Education Sciences, U.S. Department of Education, through Grant [R305A190100](https://ies.ed.gov/use-work/awards/identifying-optimal-scoring-metrics-and-prompt-type-written-expression-curriculum-based-measurement?ID=3339). The opinions expressed are those of the authors and do not represent views of the Institute or the U.S. Department of Education. Principal Investigator: Milena Keller-Margulis (University of Houston). Co-Principal Investigator: Sterett Mercer (University of British Columbia). Co-Principal Investigator: Jorge Gonzalez (University of Houston). Co-Investigator: Bruno Zumbo (University of British Columbia).
 * This work was supported by a Partnership Development Grant (_Assessment for Effective Intervention in Written Expression for Students with Learning Disabilities_) from the Social Sciences and Humanities Research Council of Canada. Principal Investigator: Sterett Mercer (University of British Columbia). Co-Investigators: Joanna Cannon (UBC) and Kate Raven (Learning Disabilities Society of Greater Vancouver).
