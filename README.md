# writeAlizer: R Package to Generate Automated Writing Quality and Curriculum-Based Measurement (CBM) Scores

This repository hosts code for a [R package](https://cran.r-project.org/) to apply research-based writing scoring models (see references below). In addition, this repository hosts documentation as an electronic supplement to published research articles in the repository wiki.

The writeAlizer R package (a) imports [ReaderBench](https://git.readerbench.com/ReaderBench/ReaderBench), [Coh-Metrix](http://cohmetrix.com/), and [GAMET](https://www.linguisticanalysistools.org/gamet.html) output files into R, (b) downloads existing predictive scoring models to the local machine, and (c) uses the predictive scoring models to generate predicted writing quality scores or Correct Word Sequences and Correct Minus Incorrect Word Sequences scores from the ReaderBench, Coh-Metrix, and/or GAMET files.

## Getting Started

### Prerequisites
writeAlizer accepts the following output files as inputs:
 1. ReaderBench: writeAlizer supports output files generated from the standalone version of ReaderBench that is described [here](https://git.readerbench.com/ReaderBench/ReaderBench/-/wikis/how-to/How%20to%20install%20and%20run%20readerbench) and can be downloaded from [here](http://readerbench.com/deployment). Although ReaderBench output files are comma separated (.csv), writeAlizer needs the output file to be saved in Excel format (.xlsx) for the file to be imported into R correctly.
 2. Coh-Metrix: writeAlizer supports output files from Coh-Metrix version 3.0 (.csv format).
 3. GAMET: writeAlizer supports output files from GAMET version 1.0 (.csv format).



### Installing

writeAlizer is not yet available on [CRAN](https://cran.r-project.org/). To install writeAlizer in R, first make sure that the package *devtools* is installed in R
```
install.packages("devtools")
```
With *devtools* installed, you can install writeAlizer in R directly from this Github repository.
```
devtools::install_github("shmercer/writeAlizer")
```
## Documentation

Information on the various scoring models available and how they were developed is in this respository's wiki.

A worked example with sample code is also presented in the wiki.

## Package Author

* **Sterett H. Mercer** - *University of British Columbia* - [UBC Faculty Profile](https://ecps.educ.ubc.ca/person/sterett-mercer/) - [ResearchGate](https://www.researchgate.net/profile/Sterett_Mercer)     [Google Scholar](https://scholar.google.ca/citations?user=YJg4svsAAAAJ&hl=en)

## References

### Journal Articles

Mercer, S. H., Keller-Margulis, M. A., Faith, E. L., Reid, E. K., & Ochs, S. (2019). The potential for automated text evaluation to improve the technical adequacy of written expression curriculum-based measurement. *Learning Disability Quarterly, 42*, 117-128. [https://doi.org/10.1177/0731948718803296](https://doi.org/10.1177/0731948718803296) 

### Conference Presentations

Mercer, S. H., Keller-Margulis, M. A., & Matta, M. (2020, February). _[Validity of automated vs. hand-scored written expression curriculum-based measurement samples](https://blogs.ubc.ca/mercer/2020/02/11/pcrc-2020-poster-automated-text-eval-for-screening/)._ Poster presented at the Pacific Coast Research Conference, Coronado, CA, USA.

Mercer, S. H., & Cannon, J. E. (2020, February). _[Monitoring the written expression gains of learners during intensive writing intervention](https://blogs.ubc.ca/mercer/2020/02/11/pcrc-2020-automated-text-eval-for-progress-monitoring/)._ Poster presented at the Pacific Coast Research Conference, Coronado, CA, USA.

Keller-Margulis, M. A., & Mercer, S. H. (2019, August). _[Validity of automated scoring for written expression curriculum-based measurement](https://blogs.ubc.ca/mercer/2019/12/18/ies-pi-meeting-2020/)._ Poster presented at the meeting of the American Psychological Association, Chicago, IL, USA.

Mercer, S. H., Tsiriotakis, I., Kwon, E., & Cannon, J. E. (2019, June). _[Evaluating elementary students' response to intervention in written expression](https://blogs.ubc.ca/mercer/2019/06/01/csse-2019-presentation-paper-and-slides/)._ Paper presented at the meeting of the Canadian Association for Educational Psychology (Canadian Society of the Study of Education), Vancouver, BC, Canada.

## License

This project is licensed under the GNU General Public License Version 3 ([GPLv3](https://www.gnu.org/licenses/gpl-3.0.en.html)).

## Acknowledgments

 * The research reported here was supported by the Institute of Education Sciences, U.S. Department of Education, through Grant [R305A190100](https://ies.ed.gov/funding/grantsearch/details.asp?ID=3339). The opinions expressed are those of the authors and do not represent views of the Institute or the U.S. Department of Education. Principal Investigator: [Milena Keller-Margulis](http://voyager.coe.uh.edu/dir/faculty_template.cfm?id=504) (University of Houston). Co-Principal Investigator: [Sterett Mercer](https://ecps.educ.ubc.ca/person/sterett-mercer/) (University of British Columbia). Co-Principal Investigator: [Jorge Gonzalez](http://www.uh.edu/education/about/directory/employee-profile/index.php?id=725) (University of Houston). Co-Investigator: [Bruno Zumbo](https://ecps.educ.ubc.ca/person/bruno-zumbo/) (University of British Columbia).
 * This work was supported by a Partnership Development Grant (_Assessment for Effective Intervention in Written Expression for Students with Learning Disabilities_) from the Social Sciences and Humanities Research Council of Canada. Principal Investigator: [Sterett Mercer](https://ecps.educ.ubc.ca/person/sterett-mercer/) (University of British Columbia). Co-Investigators: [Joanna Cannon](https://ecps.educ.ubc.ca/person/joanna-cannon/) (UBC) and [Kate Raven](http://ldsociety.ca/about/) (Learning Disabilities Society of Greater Vancouver).

<!--stackedit_data:
eyJoaXN0b3J5IjpbLTE4ODI4MTU3MTgsMzExNTIzMTg5LDc0OT
k1MjEzMywtOTMyOTUxNTQ2LC04MzY5MTE4NDIsLTE5OTg4NTIy
NDQsLTg1Nzg1NDE2NCwtMTA1NTI2ODAwOCwtMjg3NjUxMDY0LC
0xNjY1MjMxMjAwLDc0Nzc4NzAyNV19
-->