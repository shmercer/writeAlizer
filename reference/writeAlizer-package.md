# writeAlizer: An R Package to Generate Automated Writing Quality and Curriculum-Based Measurement (CBM) Scores.

Package-level documentation for writeAlizer.

## Details

Detailed documentation on writeAlizer is available in the [GitHub README
file](https://github.com/shmercer/writeAlizer) and
[wiki](https://github.com/shmercer/writeAlizer/wiki).

The writeAlizer R package (a) imports
[ReaderBench](https://github.com/readerbench/readerbench-java),
[Coh-Metrix](https://soletlab.asu.edu/coh-metrix/), and
[GAMET](https://www.linguisticanalysistools.org/gamet.html) output files
into R, and (b) uses research-developed scoring models to generate
predicted writing quality scores or Correct Word Sequences and Correct
Minus Incorrect Word Sequences scores from those files.

The writeAlizer package includes functions to do two types of tasks: (1)
importing ReaderBench, Coh-Metrix, and/or GAMET output files into R; and
(2) generating predicted quality scores using the imported output files.
There are also additional functions to help with (3) installation of
package dependencies and (4) cache management.

## 1. Import output files

- [`import_rb`](https://shmercer.github.io/writeAlizer/reference/import_rb.md)

- [`import_coh`](https://shmercer.github.io/writeAlizer/reference/import_coh.md)

- [`import_gamet`](https://shmercer.github.io/writeAlizer/reference/import_gamet.md)

- [`import_merge_gamet_rb`](https://shmercer.github.io/writeAlizer/reference/import_merge_gamet_rb.md)

## 2. Generate predicted quality scores

- [`predict_quality`](https://shmercer.github.io/writeAlizer/reference/predict_quality.md)

## 3. Identify necessary packages

- [`model_deps`](https://shmercer.github.io/writeAlizer/reference/model_deps.md)

## 4. Cache management

- [`wa_cache_dir`](https://shmercer.github.io/writeAlizer/reference/wa_cache_dir.md)

- [`wa_cache_clear`](https://shmercer.github.io/writeAlizer/reference/wa_cache_clear.md)

## See also

Useful links:

- <https://github.com/shmercer/writeAlizer/>

- <https://shmercer.github.io/writeAlizer/>

- Report bugs at <https://github.com/shmercer/writeAlizer/issues>

## Author

**Maintainer**: Sterett H. Mercer <sterett.mercer@ubc.ca>
([ORCID](https://orcid.org/0000-0002-7940-4221))
