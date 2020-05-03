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

writeAlizer is not yet available on CRAN. To install writeAlizer in R, first make sure that the package *devtools* is installed in R
```
install.packages("devtools")
```
With *devtools* installed, you can install writeAlizer in R directly from this Github repository.
```
devtools::install_github("shmercer/writeAlizer")
```
## Running the tests

Explain how to run the automated tests for this system

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

## Built With

* [Dropwizard](http://www.dropwizard.io/1.0.2/docs/) - The web framework used
* [Maven](https://maven.apache.org/) - Dependency Management
* [ROME](https://rometools.github.io/rome/) - Used to generate RSS Feeds

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Billie Thompson** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## References

### Journal Articles

Mercer, S. H., Keller-Margulis, M. A., Faith, E. L., Reid, E. K., & Ochs, S. (2019). The potential for automated text evaluation to improve the technical adequacy of written expression curriculum-based measurement. *Learning Disability Quarterly, 42*, 117-128. [https://doi.org/10.1177/0731948718803296](https://doi.org/10.1177/0731948718803296) 

### Conference Presentations

Mercer, S. H., Keller-Margulis, M. A., & Matta, M. (2020, February). _[Validity of automated vs. hand-scored written expression curriculum-based measurement samples](https://blogs.ubc.ca/mercer/2020/02/11/pcrc-2020-poster-automated-text-eval-for-screening/)._ Poster presented at the Pacific Coast Research Conference, Coronado, CA, USA.

Mercer, S. H., & Cannon, J. E. (2020, February). _[Monitoring the written expression gains of learners during intensive writing intervention](https://blogs.ubc.ca/mercer/2020/02/11/pcrc-2020-automated-text-eval-for-progress-monitoring/)._ Poster presented at the Pacific Coast Research Conference, Coronado, CA, USA.

Keller-Margulis, M. A., & Mercer, S. H. (2019, August). _[Validity of automated scoring for written expression curriculum-based measurement](https://blogs.ubc.ca/mercer/2019/12/18/ies-pi-meeting-2020/)._ Poster presented at the meeting of the American Psychological Association, Chicago, IL, USA.

Mercer, S. H., Tsiriotakis, I., Kwon, E., & Cannon, J. E. (2019, June). _[Evaluating elementary students' response to intervention in written expression](https://blogs.ubc.ca/mercer/2019/06/01/csse-2019-presentation-paper-and-slides/)._ Paper presented at the meeting of the Canadian Association for Educational Psychology (Canadian Society of the Study of Education), Vancouver, BC, Canada.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Hat tip to anyone whose code was used
* Inspiration
* etc
<!--stackedit_data:
eyJoaXN0b3J5IjpbNDg2Mjc1OTgwLC0xOTk4ODUyMjQ0LC04NT
c4NTQxNjQsLTEwNTUyNjgwMDgsLTI4NzY1MTA2NCwtMTY2NTIz
MTIwMCw3NDc3ODcwMjVdfQ==
-->