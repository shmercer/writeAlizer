# writeAlizer: An R Package to Generate Automated Writing Quality and Curriculum-Based Measurement (CBM) Scores

This repository hosts code for an [R package](https://cran.r-project.org/) to apply research-based writing scoring models (see references below). In addition, this repository hosts documentation as an electronic supplement to published research articles in the repository wiki.

The writeAlizer R package (a) imports [ReaderBench](http://readerbench.com/), [Coh-Metrix](http://cohmetrix.com/), and [GAMET](https://www.linguisticanalysistools.org/gamet.html) output files into R, (b) downloads existing predictive scoring models to the local machine, and (c) uses the predictive scoring models to generate predicted writing quality scores or Correct Word Sequences and Correct Minus Incorrect Word Sequences scores from the ReaderBench, Coh-Metrix, and/or GAMET files.

### Versions
The version history of writeAlizer is available in the package [NEWS.md](https://github.com/shmercer/writeAlizer/blob/master/NEWS.md) file.

## Getting Started

### Prerequisites
writeAlizer accepts the following output files as inputs:
 1. ReaderBench: writeAlizer supports output files (.csv format) generated from the standalone version of ReaderBench that can be downloaded from [here](http://readerbench.com/deployment).
 2. Coh-Metrix: writeAlizer supports output files from Coh-Metrix version 3.0 (.csv format).
 3. GAMET: writeAlizer supports output files from GAMET version 1.0 (.csv format).

The writeAlizer scoring models assume that column names in the output files have been unchanged (exactly the same as generated from the program). For programs that list file paths in the first column, the writeAlizer file import functions will parse the file names from the file paths and store the file names as an identification variable (ID). File names/ID variables need to be numeric.

### Installing

writeAlizer is not available on [CRAN](https://cran.r-project.org/). To install writeAlizer in R, first make sure that the package *devtools* is installed in R
```
install.packages("devtools")
```
With *devtools* installed, you can install writeAlizer in R directly from this Github repository.
```
devtools::install_github("shmercer/writeAlizer")
```
After installation, documentation of the file import and predict_quality() functions, and examples of their use, can be found in the R package help file.
```
help("writeAlizer")
```

## Documentation

Information on the various scoring models available and how they were developed is in this respository's [wiki](https://github.com/shmercer/writeAlizer/wiki):

1. [Description of the general process used to develop scoring algorithms](https://github.com/shmercer/writeAlizer/wiki/Scoring-Model-Development).
2. Description of the following specific scoring models (models recommended for use in research are indicated by *), including information on the relative importance of metrics and weighting of algorithms:
   * [Coh-Metrix Model 1](https://github.com/shmercer/writeAlizer/wiki/CohMetrix-Model-1)
   * [Coh-Metrix Model 2*](https://github.com/shmercer/writeAlizer/wiki/CohMetrix-Model-2)
   * [ReaderBench Model 1](https://github.com/shmercer/writeAlizer/wiki/ReaderBench-Model-1)
   * [ReaderBench Model 2*](https://github.com/shmercer/writeAlizer/wiki/ReaderBench-Model-2)
   * [Automated Written Expression CBM Model 1](https://github.com/shmercer/writeAlizer/wiki/aWE-CBM-Model-1)

## Package Author and Maintainer

* **Sterett H. Mercer** - *University of British Columbia* - [UBC Faculty Profile](https://ecps.educ.ubc.ca/person/sterett-mercer/) - [ResearchGate](https://www.researchgate.net/profile/Sterett_Mercer) - [Google Scholar](https://scholar.google.ca/citations?user=YJg4svsAAAAJ&hl=en)

Also see the list of code [contributors](https://github.com/shmercer/writeAlizer/contributors) for this package.

## References

### Journal Articles

Matta, M., Mercer, S. H., & Keller-Margulis, M. A. (in press). Evaluating validity and bias for hand-calculated and automated written expression curriculum-based measurement scores. *Assessment in Education: Principles, Policy & Practice*. [https://doi.org/10.1080/0969594X.2022.2043240](https://doi.org/10.1080/0969594X.2022.2043240)

Mercer, H. H., & Cannon, J. E. (2022). Validity of automated learning progress assessment in English written expression for students with learning difficulties. *Journal for Educational Research Online, 14*, 39-60. [https://doi.org/10.31244/jero.2022.01.03](https://doi.org/10.31244/jero.2022.01.03)

Matta, M., Keller-Margulis, M. A., & Mercer, S. H. (2022). Cost analysis and cost effectiveness of hand-scored and automated approaches to writing screening. *Journal of School Psychology, 92*, 80-95. [https://doi.org/10.1016/j.jsp.2022.03.003](https://doi.org/10.1016/j.jsp.2022.03.003)

Keller-Margulis, M. A., Mercer, S. H., & Matta, M. (2021). Validity of automated text evaluation tools for written-expression curriculum-based measurement: A comparison study. *Reading and Writing: An Interdisciplinary Journal, 34*, 2461-2480. [https://doi.org/10.1007/s11145-021-10153-6](https://doi.org/10.1007/s11145-021-10153-6)  
[link to pre-print of accepted article](https://doi.org/10.31219/osf.io/gcetv)

Mercer, S. H., Cannon, J. E., Squires, B., Guo, Y., & Pinco, E. (2021). Accuracy of automated written expression curriculum-based measurement scoring. *Canadian Journal of School Psychology, 36*, 304-317. [https://doi.org/10.1177/0829573520987753](https://doi.org/10.1177/0829573520987753)
[link to pre-print of accepted article](https://doi.org/10.31219/osf.io/yrvq5)

Mercer, S. H., Keller-Margulis, M. A., Faith, E. L., Reid, E. K., & Ochs, S. (2019). The potential for automated text evaluation to improve the technical adequacy of written expression curriculum-based measurement. *Learning Disability Quarterly, 42*, 117-128. [https://doi.org/10.1177/0731948718803296](https://doi.org/10.1177/0731948718803296)

### Conference Presentations

Matta, M., Keller-Margulis M., & Mercer, S. H. (2022, February). *New directions for writing assessment: Improving feasibility with automated scoring.* Presentation at the meeting of the National Association of School Psychologists, Boston, MA, USA.

Matta, M., Keller-Margulis, M., & Mercer, S. H. (2021, July). *The use of automated approaches to scoring written expression of elementary students.* Poster presented at the at the meeting of the International School Psychology Association, online.

Matta, Michael, Keller-Margulis, M. A., Mercer, S. H., & Zopatti, K. (2021, February). *Improving written-expression curriculum-based measurement feasibility with automated text evaluation programs.* Paper presented at the meeting of the National Association of School Psychologists, online.

Mercer, S. H., Keller-Margulis, M. A., & Matta, M. (2020, February). _[Validity of automated vs. hand-scored written expression curriculum-based measurement samples](https://blogs.ubc.ca/mercer/2020/02/11/pcrc-2020-poster-automated-text-eval-for-screening/)._ Poster presented at the Pacific Coast Research Conference, Coronado, CA, USA.

Mercer, S. H., & Cannon, J. E. (2020, February). _[Monitoring the written expression gains of learners during intensive writing intervention](https://blogs.ubc.ca/mercer/2020/02/11/pcrc-2020-automated-text-eval-for-progress-monitoring/)._ Poster presented at the Pacific Coast Research Conference, Coronado, CA, USA.

Keller-Margulis, M. A., & Mercer, S. H. (2019, August). _[Validity of automated scoring for written expression curriculum-based measurement](https://blogs.ubc.ca/mercer/2019/12/18/ies-pi-meeting-2020/)._ Poster presented at the meeting of the American Psychological Association, Chicago, IL, USA.

Mercer, S. H., Tsiriotakis, I., Kwon, E., & Cannon, J. E. (2019, June). _[Evaluating elementary students' response to intervention in written expression](https://blogs.ubc.ca/mercer/2019/06/01/csse-2019-presentation-paper-and-slides/)._ Paper presented at the meeting of the Canadian Association for Educational Psychology (Canadian Society of the Study of Education), Vancouver, BC, Canada.

## License

This project is licensed under the GNU General Public License Version 3 ([GPLv3](LICENSE)).

## Acknowledgments

 * The research reported here was supported by the Institute of Education Sciences, U.S. Department of Education, through Grant [R305A190100](https://ies.ed.gov/funding/grantsearch/details.asp?ID=3339). The opinions expressed are those of the authors and do not represent views of the Institute or the U.S. Department of Education. Principal Investigator: [Milena Keller-Margulis](http://voyager.coe.uh.edu/dir/faculty_template.cfm?id=504) (University of Houston). Co-Principal Investigator: [Sterett Mercer](https://ecps.educ.ubc.ca/person/sterett-mercer/) (University of British Columbia). Co-Principal Investigator: [Jorge Gonzalez](http://www.uh.edu/education/about/directory/employee-profile/index.php?id=725) (University of Houston). Co-Investigator: [Bruno Zumbo](https://ecps.educ.ubc.ca/person/bruno-zumbo/) (University of British Columbia).
 * This work was supported by a Partnership Development Grant (_Assessment for Effective Intervention in Written Expression for Students with Learning Disabilities_) from the Social Sciences and Humanities Research Council of Canada. Principal Investigator: [Sterett Mercer](https://ecps.educ.ubc.ca/person/sterett-mercer/) (University of British Columbia). Co-Investigators: [Joanna Cannon](https://ecps.educ.ubc.ca/person/joanna-cannon/) (UBC) and [Kate Raven](http://ldsociety.ca/about/) (Learning Disabilities Society of Greater Vancouver).
