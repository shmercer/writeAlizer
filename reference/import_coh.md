# Import a Coh-Metrix output file (.csv) into R.

Import a Coh-Metrix output file (.csv) into R.

## Usage

``` r
import_coh(path)
```

## Arguments

- path:

  A string giving the path and filename to import.

## Value

A base `data.frame` with one row per record and the following columns:

- `ID` (`character`): unique identifier of the text/essay.

- One column per retained Coh*-*Metrix feature, kept by original feature
  name (`numeric`). Feature names mirror the Coh*-*Metrix output
  variables.

The object has class `data.frame` (or `tibble` if converted by the
user).

## See also

[`predict_quality`](https://shmercer.github.io/writeAlizer/reference/predict_quality.md)

## Examples

``` r
# Example with package sample data
file_path <- system.file("extdata", "sample_coh.csv", package = "writeAlizer")
coh_file  <- import_coh(file_path)
head(coh_file)
#>   ID DESPC DESSC DESWC DESPL DESPLd  DESSL DESSLd DESWLsy DESWLsyd DESWLlt
#> 1  7    11    13   135 1.182  0.405 10.385  4.464   1.296    0.681   4.030
#> 2  8    13    18   175 1.385  0.506  9.833  5.090   1.291    0.558   4.120
#> 3  9    17    22   193 1.294  0.470  9.045  5.131   1.316    0.585   4.285
#>   DESWLltd PCNARz PCNARp PCSYNz PCSYNp PCCNCz PCCNCp PCREFz PCREFp PCDCz PCDCp
#> 1    2.091  0.644  73.89  0.565  71.23 -0.865  19.49 -1.556   6.06 1.767 96.08
#> 2    2.021  0.364  64.06  1.480  92.92  1.717  95.64 -2.322   1.02 1.674 95.25
#> 3    2.384  0.597  72.24  0.527  69.85  1.432  92.36 -2.186   1.46 1.005 84.13
#>   PCVERBz PCVERBp PCCONNz PCCONNp PCTEMPz PCTEMPp CRFNO1 CRFAO1 CRFSO1 CRFNOa
#> 1   1.101   86.43  -0.067   47.61   0.832   79.67  0.083  0.167  0.083  0.067
#> 2  -1.596    5.59  -5.314    0.00   0.363   64.06  0.000  0.000  0.000  0.048
#> 3  -0.606   27.43  -4.613    0.00   1.413   92.07  0.000  0.095  0.000  0.024
#>   CRFAOa CRFSOa CRFCWO1 CRFCWO1d CRFCWOa CRFCWOad CRFANP1 CRFANPa LSASS1
#> 1  0.107  0.080   0.063    0.151   0.035    0.083   0.167   0.053  0.104
#> 2  0.112  0.056   0.000    0.000   0.026    0.077   0.235   0.104  0.073
#> 3  0.152  0.024   0.017    0.054   0.026    0.064   0.190   0.085  0.084
#>   LSASS1d LSASSp LSASSpd LSAPP1 LSAPP1d LSAGN LSAGNd LDTTRc LDTTRa  LDMTLD
#> 1   0.111  0.118   0.118  0.100   0.118 0.233  0.147  0.836  0.659  63.043
#> 2   0.112  0.025   0.071  0.102   0.098 0.215  0.092  0.888  0.621  98.598
#> 3   0.121  0.089   0.128  0.093   0.109 0.215  0.096  0.919  0.658 108.407
#>    LDVOCD  CNCAll CNCCaus CNCLogic CNCADC CNCTemp CNCTempx CNCAdd  CNCPos
#> 1  80.677  88.889  44.444   51.852  7.407  29.630   22.222 22.222  81.481
#> 2  87.942 125.714  28.571   74.286 34.286  17.143   17.143 74.286  91.429
#> 3 105.874 124.352  36.269   36.269 20.725  10.363   10.363 72.539 103.627
#>   CNCNeg SMCAUSv SMCAUSvp SMINTEp SMCAUSr SMINTEr SMCAUSlsa SMCAUSwn SMTEMP
#> 1  7.407  37.037   51.852  22.222   0.333   1.000     0.073    0.439  0.917
#> 2 34.286  51.429   62.857  28.571   0.200   0.833     0.040    0.297  0.853
#> 3 15.544  36.269   62.176  31.088   0.625   0.857     0.051    0.359  0.952
#>   SYNLE SYNNP SYNMEDpos SYNMEDwrd SYNMEDlem SYNSTRUTa SYNSTRUTt    DRNP    DRVP
#> 1 0.846 0.703     0.742     0.938     0.938     0.065     0.064 370.370 229.630
#> 2 1.500 0.673     0.891     0.975     0.975     0.045     0.066 337.143 251.429
#> 3 1.545 0.820     0.772     0.973     0.948     0.071     0.081 326.425 248.705
#>     DRAP    DRPP DRPVAL  DRNEG DRGERUND  DRINF WRDNOUN WRDVERB WRDADJ WRDADV
#> 1 44.444 140.741  7.407 14.815    7.407 22.222 214.815 148.147 81.481 59.259
#> 2 28.571  91.429 11.429  5.714   22.857 17.143 211.429 154.285 91.428 34.286
#> 3 41.451  72.539  0.000 10.363   25.907 20.725 207.254 155.441 88.083 51.813
#>    WRDPRO WRDPRP1s WRDPRP1p WRDPRP2 WRDPRP3s WRDPRP3p WRDFRQc WRDFRQa WRDFRQmc
#> 1  51.852    7.407        0   0.000   44.444    0.000   2.720   3.331    1.703
#> 2  91.429    0.000        0   5.714   74.286    0.000   2.095   3.013    1.362
#> 3 103.627    0.000        0   0.000   82.902   10.363   2.115   3.051    1.442
#>   WRDAOAc WRDFAMc WRDCNCc WRDIMGc WRDMEAc WRDPOLc WRDHYPn WRDHYPv WRDHYPnv
#> 1 330.250 574.958 362.043 407.979 429.614   4.020   5.358   1.676    1.394
#> 2 277.905 553.695 424.508 466.831 439.127   3.943   6.508   1.797    1.628
#> 3 292.227 564.014 408.095 444.754 439.589   4.959   6.508   1.601    1.502
#>    RDFRE RDFKGL   RDL2
#> 1 86.653  3.753 22.648
#> 2 87.749  3.435  4.236
#> 3 86.597  3.360  7.163
```
