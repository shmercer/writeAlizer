![writeAlizer Logo](https://www.dropbox.com/s/bxgse42uf44k6me/wA_logo.png?raw=1)

# writeAlizer: An R Package to Generate Automated Writing Quality and Curriculum-Based Measurement (CBM) Scores

<!-- badges: start -->
  [![CRAN status](https://www.r-pkg.org/badges/version/writeAlizer)](https://CRAN.R-project.org/package=writeAlizer)
  [![R-CMD-check](https://github.com/shmercer/writeAlizer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/shmercer/writeAlizer/actions/workflows/R-CMD-check.yaml)
  [![codecov](https://codecov.io/gh/shmercer/writeAlizer/branch/master/graph/badge.svg)](https://app.codecov.io/gh/shmercer/writeAlizer)
  [![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE.md)
<!-- badges: end -->

This repository hosts code for an [R package](https://cran.r-project.org/) to apply research-based writing scoring models (see references below). In addition, this repository hosts documentation as an electronic supplement to published research articles in the [repository wiki](https://github.com/shmercer/writeAlizer/wiki).

The writeAlizer R package (a) imports [ReaderBench](https://github.com/readerbench/readerbench-java), [Coh-Metrix](https://soletlab.asu.edu/coh-metrix/), and [GAMET](https://www.linguisticanalysistools.org/gamet.html) output files into R, (b) downloads existing predictive scoring models to the local machine, and (c) uses the predictive scoring models to generate predicted writing quality scores or Correct Word Sequences and Correct Minus Incorrect Word Sequences scores from the ReaderBench, Coh-Metrix, and/or GAMET files.

### Versions
The version history of writeAlizer is available in the package [NEWS.md](https://github.com/shmercer/writeAlizer/blob/master/NEWS.md) file.

## Getting Started

### Prerequisites
writeAlizer accepts the following output files as inputs:
 1. ReaderBench: writeAlizer supports output files (.csv format) generated from the Java version of ReaderBench. [Source Code](https://github.com/readerbench/readerbench-java) [Windows Binaries](https://osf.io/wyq4t)
 2. Coh-Metrix: writeAlizer supports output files from Coh-Metrix version 3.0 (.csv format).
 3. GAMET: writeAlizer supports output files from GAMET version 1.0 (.csv format).

The writeAlizer scoring models assume that column names in the output files have been unchanged (exactly the same as generated from the program). For programs that list file paths in the first column, the writeAlizer file import functions will parse the file names from the file paths and store the file names as an identification variable (ID). `import_rb()` (ReaderBench) and `import_coh()` (Coh-Metrix) keep IDs as **character**. For ReaderBench CSVs, the original `File.name` column is renamed to `ID` and stored as character. Numeric IDs are fine too, but they are not coerced to numeric to avoid losing leading zeros or other formatting.

### Installing

``` r
# To install from CRAN:
install.packages("writeAlizer")

# for documentation of the file import and predict_quality() functions
help("writeAlizer")
```

### Development version

``` r
#install.packages("devtools")
devtools::install_github("shmercer/writeAlizer")

```

### Install model dependencies (Suggests)

Some models rely on packages listed in `Suggests`. use `model_deps()` to discover what’s needed on your machine to run those models locally.

```r
# Discover optional model packages from writeAlizer's Suggests
md <- writeAlizer::model_deps()

md$required
md$missing
```
`model_deps()` also prints a helpful message. If anything is missing, it includes a copy-paste command like:

```r
Missing required packages: glmnet, ranger
Install them manually, e.g.:
  install.packages(c("glmnet", "ranger"))
```

### Quickstart: generate predicted quality scores with the rb_mod3all / coh_mod3all models

```r
library(writeAlizer)

## ReaderBench example
rb_path <- system.file("extdata", "sample_rb.csv", package = "writeAlizer") #read path of included sample rb output file
rb <- import_rb(rb_path) #import the rb file
rb_pred <- predict_quality(model = "rb_mod3all", rb) #generate predicted values

## Coh-Metrix example

coh_path <- system.file("extdata", "sample_coh.csv", package = "writeAlizer") #read path of included sample Cooh-Metrix output file
coh <- import_coh(coh_path) #import the file
coh_pred <- predict_quality(model = "coh_mod3all", coh) #generate predicted values
```

### About predict_quality() output

Some models are ensembles and will output multiple sub-predictions (e.g., genre-specific or component models). In those cases, predict_quality() adds a column named pred_model_mean, which is the mean of that model’s sub-predictions. For single-output models, you’ll just see the pred_model column.

### Where model files are stored

By default, writeAlizer caches downloaded model artifacts in a user cache directory.

```r
# return the cache directory location
wa_cache_dir()

# list objects in the cache, with option to clear it
wa_cache_clear()
```

## Documentation

Information on the various scoring models available and how they were developed is in this repository's [wiki](https://github.com/shmercer/writeAlizer/wiki):

1. [Description of the general process used to develop scoring algorithms](https://github.com/shmercer/writeAlizer/wiki/Scoring-Model-Development).
2. Description of the following specific scoring models (models recommended for use in research are indicated by *), including information on the relative importance of metrics and weighting of algorithms:
   * [Coh-Metrix Model 1](https://github.com/shmercer/writeAlizer/wiki/CohMetrix-Model-1)
   * [Coh-Metrix Model 2](https://github.com/shmercer/writeAlizer/wiki/CohMetrix-Model-2)
   * [Coh-Metrix Model 3*](https://github.com/shmercer/writeAlizer/wiki/CohMetrix-Model-3)
   * [ReaderBench Model 1](https://github.com/shmercer/writeAlizer/wiki/ReaderBench-Model-1)
   * [ReaderBench Model 2](https://github.com/shmercer/writeAlizer/wiki/ReaderBench-Model-2)
   * [ReaderBench Model 3*](https://github.com/shmercer/writeAlizer/wiki/ReaderBench-Model-3)
   * [Automated Written Expression CBM Model 1](https://github.com/shmercer/writeAlizer/wiki/aWE-CBM-Model-1)

## Package Author and Maintainer

* **Sterett H. Mercer** - *University of British Columbia*  
UBC Faculty Profile: https://ecps.educ.ubc.ca/sterett-h-mercer/  
ResearchGate: https://www.researchgate.net/profile/Sterett_Mercer  
Google Scholar: https://scholar.google.ca/citations?user=YJg4svsAAAAJ&hl=en  

Also see the list of code [contributors](https://github.com/shmercer/writeAlizer/contributors) for this package.

## References

### Journal Articles

Matta, M., Keller-Margulis, M. A., & Mercer, S. H. (in press). Improving written-expression curriculum-based measurement feasibility with automated text evaluation programs. *School Psychology.* https://doi.org/10.1037/spq0000691

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

This project is licensed under the MIT License. See [LICENSE.md](LICENSE.md) for details.

## Acknowledgments

 * The research reported here was supported by the Institute of Education Sciences, U.S. Department of Education, through Grant [R305A190100](https://ies.ed.gov/use-work/awards/identifying-optimal-scoring-metrics-and-prompt-type-written-expression-curriculum-based-measurement?ID=3339). The opinions expressed are those of the authors and do not represent views of the Institute or the U.S. Department of Education. Principal Investigator: Milena Keller-Margulis (University of Houston). Co-Principal Investigator: Sterett Mercer (University of British Columbia). Co-Principal Investigator: Jorge Gonzalez (University of Houston). Co-Investigator: Bruno Zumbo (University of British Columbia).
 * This work was supported by a Partnership Development Grant (_Assessment for Effective Intervention in Written Expression for Students with Learning Disabilities_) from the Social Sciences and Humanities Research Council of Canada. Principal Investigator: Sterett Mercer (University of British Columbia). Co-Investigators: Joanna Cannon (UBC) and Kate Raven (Learning Disabilities Society of Greater Vancouver).
