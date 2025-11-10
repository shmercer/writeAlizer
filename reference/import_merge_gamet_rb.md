# Import a ReaderBench output file (.csv) and GAMET output file (.csv), and merge the two files on ID.

Import a ReaderBench output file (.csv) and GAMET output file (.csv),
and merge the two files on ID.

## Usage

``` r
import_merge_gamet_rb(rb_path, gamet_path)
```

## Arguments

- rb_path:

  A string giving the path and ReaderBench filename to import.

- gamet_path:

  A string giving the path and GAMET filename to import.

## Value

A base `data.frame` created by joining the ReaderBench and GAMET tables
by `ID`, with one row per matched ID and the following columns:

- `ID` (`character`): identifier present in both sources.

- All retained ReaderBench feature columns (`numeric`).

- All retained GAMET error/category columns (`numeric`).

By default, only IDs present in both inputs are kept (inner join). If a
feature name appears in both sources, standard merge suffixes (e.g.,
`.x`/`.y`) may be applied by the join implementation. The object has
class `data.frame` (or `tibble` if converted by the user).

## See also

[`predict_quality`](https://shmercer.github.io/writeAlizer/reference/predict_quality.md)

## Examples

``` r
# Example with package sample data
rb_path   <- system.file("extdata", "sample_rb.csv", package = "writeAlizer")
gam_path  <- system.file("extdata", "sample_gamet.csv", package = "writeAlizer")
rb_gam    <- import_merge_gamet_rb(rb_path, gam_path)
head(rb_gam)
#>   ID error_count word_count grammar misspelling duplication typographical
#> 1  7           6        135       0           6           0             0
#> 2  8           4        171       0           3           0             1
#> 3  9           3        191       0           2           0             1
#>   whitespace per_gram  per_spell Paragraphs Sentences Words Content.words
#> 1          0        0 0.04444444         11        13   135            48
#> 2          0        0 0.01754386         13        18   168            69
#> 3          0        0 0.01047120         17        22   189            81
#>   RB.RdbltyFlesch RB.RdbltyFog RB.RdbltyKincaid RB.RdbltyDaleChall RB.AvgBlLen
#> 1        61.61668     14.65185        11.372593           7.660466    23.09091
#> 2        68.87622     11.35702         8.962624           7.727356    28.23077
#> 3        66.90096     10.09312         9.144974           7.952616    25.52941
#>   RB.AvgCommaBl RB.AvgCommaSen RB.AvgSenLen RB.AvgSenBl RB.AvgUnqWdBl
#> 1     0.8181818      0.6923077     19.53846    1.181818      4.272727
#> 2     0.6923077      0.5000000     20.38889    1.384615      5.307692
#> 3     0.9411765      0.7272727     19.72727    1.294118      4.705882
#>   RB.AvgUnqWdSen RB.AvgWdLen RB.AvgWdBl RB.AvgWdSen RB.CharEnt RB.BlStDevSen
#> 1       3.615385    5.291667   4.363636    3.692308   3.029485     0.4045199
#> 2       3.833333    5.318841   5.307692    3.833333   2.972067     0.5063697
#> 3       3.681818    5.358025   4.764706    3.681818   2.948719     0.4696682
#>   RB.BlStDevUnq RB.SenStDevUnqWd RB.BlStdDevWd RB.SenStdDevWd RB.WdEnt
#> 1      1.737292         1.660244      1.804036       1.750458 4.240761
#> 2      2.136376         2.357716      2.136376       2.357716 4.356472
#> 3      2.494111         2.378202      2.537947       2.378202 4.496810
#>   RB.WdLettStdDev RB.LxcDiv RB.LxcSoph RB.SynDiv RB.SynSoph   RB.CAF
#> 1        2.041676  3.742771   3.916667 0.4893617   4.285714 6.443732
#> 2        1.693350  4.574957   3.825581 0.4779412   4.526316 7.145781
#> 3        1.919040  4.925183   3.960396 0.4937500   4.391304 7.189784
#>   RB.AvgNounBl RB.AvgPronounBl RB.AvgVerbBl RB.AvgAdverbBl RB.AvgAdjectiveBl
#> 1     2.818182       0.6363636     2.363636      0.7272727         1.0000000
#> 2     2.846154       1.3076923     2.692308      0.6923077         0.7692308
#> 3     2.470588       1.1764706     2.117647      0.6470588         0.7647059
#>   RB.AvgPrepositionBl RB.AvgNounSen RB.AvgPronounSen RB.AvgVerbSen
#> 1            1.909091      2.384615        0.5384615      2.000000
#> 2            1.230769      2.055556        0.9444444      1.944444
#> 3            1.235294      1.909091        0.9090909      1.636364
#>   RB.AvgAdverbSen RB.AvgAdjectiveSen RB.AvgPrepositionSen RB.AvgUnqNoundBl
#> 1       0.6153846          0.8461538            1.6153846         2.727273
#> 2       0.5000000          0.5555556            0.8888889         2.846154
#> 3       0.5000000          0.5909091            0.9545455         2.470588
#>   RB.AvgUnqPronounBl RB.AvgUnqVerbBl RB.AvgUnqAdverbBl RB.AvgUnqAdjectiveBl
#> 1          0.6363636        2.181818         0.7272727            1.0000000
#> 2          1.1538462        2.615385         0.6923077            0.7692308
#> 3          1.1176471        2.000000         0.6470588            0.7647059
#>   RB.AvgUnqPrepositionBl RB.AvgPronBl_first_person RB.AggPronSen_first_person
#> 1               1.727273                0.09090909                 0.07692308
#> 2               1.153846                0.00000000                 0.00000000
#> 3               1.235294                0.00000000                 0.00000000
#>   RB.AvgPronBl_indefinite RB.AggPronSen_indefinite RB.AvgPronBl_interrogative
#> 1               0.8181818                0.6923077                  0.2727273
#> 2               0.7692308                0.5555556                  0.0000000
#> 3               0.4117647                0.3181818                  0.0000000
#>   RB.AggPronSen_interrogative RB.AvgPronBl_second_person
#> 1                   0.2307692                 0.00000000
#> 2                   0.0000000                 0.07692308
#> 3                   0.0000000                 0.00000000
#>   RB.AggPronSen_second_person RB.AvgPronBl_third_person
#> 1                  0.00000000                 0.5454545
#> 2                  0.05555556                 1.2307692
#> 3                  0.00000000                 1.1764706
#>   RB.AggPronSen_third_person RB.AvgSemDep RB.WdDiffLemmaStem RB.WdDiffWdStem
#> 1                  0.4615385     11.84615          0.5000000       0.7708333
#> 2                  0.8888889     11.22222          0.3043478       0.7826087
#> 3                  0.9090909     10.80952          0.2469136       0.6543210
#>   RB.WdMaxDpthHypernymTree RB.WdAvgDpthHypernymTree RB.WdPathCntHypernymTree
#> 1                 3.833333                 3.625000                0.7708333
#> 2                 5.449275                 5.275362                0.9855072
#> 3                 5.049383                 4.925926                0.8765432
#>   RB.WdPolysemyCnt RB.WdSylCnt RB.AvgAOADoc_Shock RB.AvgAOABl_Shock
#> 1         6.708333    1.479167           4.037000          2.584394
#> 2         7.666667    1.420290           4.092000          2.886282
#> 3         9.530864    1.432099           3.728421          2.584020
#>   RB.AvgAOASen_Shock RB.AvgAOADoc_Cortese RB.AvgAOABl_Cortese
#> 1           2.422308             3.006667            2.683939
#> 2           2.084537             3.342222            3.053974
#> 3           1.996742             2.988000            2.621569
#>   RB.AvgAOASen_Cortese RB.AvgAOADoc_Kuperman RB.AvgAOABl_Kuperman
#> 1             2.485769              5.750000             6.146818
#> 2             3.058796              6.337794             6.682091
#> 3             2.744773              5.565195             5.327396
#>   RB.AvgAOASen_Kuperman RB.AvgAOADoc_Bird RB.AvgAOABl_Bird RB.AvgAOASen_Bird
#> 1              5.908218          315.0000         292.7652          293.7538
#> 2              6.420403          312.8214         278.1859          217.6620
#> 3              5.260277          313.6977         247.8324          259.7871
#>   RB.AvgAOADoc_Bristol RB.AvgAOABl_Bristol RB.AvgAOASen_Bristol
#> 1             335.7059            275.6515             233.2436
#> 2             295.8684            252.9314             229.7738
#> 3             300.7647            257.9461             214.3674
#>   RB.AvgAOEDoc_IndexPolynomialFitAboveThreshold.0.3.
#> 1                                           2.391304
#> 2                                           3.014493
#> 3                                           3.111111
#>   RB.AvgAOEBl_IndexPolynomialFitAboveThreshold.0.3.
#> 1                                          2.597835
#> 2                                          3.347436
#> 3                                          3.060317
#>   RB.AvgAOESen_IndexPolynomialFitAboveThreshold.0.3.
#> 1                                           2.453297
#> 2                                           3.126058
#> 3                                           2.893903
#>   RB.AvgAOEDoc_InverseLinearRegressionSlope
#> 1                                  1.249280
#> 2                                  1.357084
#> 3                                  1.383570
#>   RB.AvgAOEBl_InverseLinearRegressionSlope
#> 1                                 1.233930
#> 2                                 1.396308
#> 3                                 1.333983
#>   RB.AvgAOESen_InverseLinearRegressionSlope
#> 1                                  1.221486
#> 2                                  1.379351
#> 3                                  1.309603
#>   RB.AvgAOEDoc_InflectionPointPolynomial RB.AvgAOEBl_InflectionPointPolynomial
#> 1                               5.546945                              5.408585
#> 2                               6.250595                              6.473423
#> 3                               6.243204                              6.079442
#>   RB.AvgAOESen_InflectionPointPolynomial RB.AvgAOEDoc_InverseAverage
#> 1                               5.378199                   0.4697780
#> 2                               6.448596                   0.5345470
#> 3                               5.907647                   0.5288327
#>   RB.AvgAOEBl_InverseAverage RB.AvgAOESen_InverseAverage
#> 1                  0.4590395                   0.4550586
#> 2                  0.5567262                   0.5520732
#> 3                  0.5172046                   0.4992655
#>   RB.AvgAOEDoc_IndexAboveThreshold.0.3. RB.AvgAOEBl_IndexAboveThreshold.0.3.
#> 1                              1.239130                             1.108658
#> 2                              1.710145                             2.013095
#> 3                              1.555556                             1.499346
#>   RB.AvgAOESen_IndexAboveThreshold.0.3. RB.AvgNmdEntBl RB.AvgNounNmdEntBl
#> 1                              1.016300      0.3636364          0.3636364
#> 2                              1.739749      0.2307692          0.2307692
#> 3                              1.490188      0.4117647          0.2352941
#>   RB.AvgUnqNmdEntBl RB.AvgNmdEntSen RB.TCorefChainDoc RB.AvgCorefChain
#> 1         0.3636364       0.3076923                 2         2.500000
#> 2         0.2307692       0.1666667                 5         2.000000
#> 3         0.4117647       0.3181818                 6         2.166667
#>   RB.AvgChainSpan RB.AvgInferenceDistChain RB.TActCorefChainWd
#> 1            25.0                    21.50          0.01481481
#> 2            20.4                     3.40          0.02976190
#> 3            20.0                    12.75          0.03174603
#>   RB.TCorefChainBigSpan RB.AvgConnBl_addition RB.AvgConnSen_addition
#> 1                     2             0.1818182              0.1538462
#> 2                     3             0.6153846              0.4444444
#> 3                     3             0.7058824              0.5454545
#>   RB.AvgConnBl_complex_subordinators RB.AvgConnSen_complex_subordinators
#> 1                                  0                                   0
#> 2                                  0                                   0
#> 3                                  0                                   0
#>   RB.AvgConnBl_concessions RB.AvgConnSen_concessions RB.AvgConnBl_conditions
#> 1                        0                         0              0.00000000
#> 2                        0                         0              0.07692308
#> 3                        0                         0              0.00000000
#>   RB.AvgConnSen_conditions RB.AvgConnBl_conjunctions RB.AvgConnSen_conjunctions
#> 1               0.00000000                 0.1818182                  0.1538462
#> 2               0.05555556                 0.5384615                  0.3888889
#> 3               0.00000000                 0.7647059                  0.5909091
#>   RB.AvgConnBl_conjuncts RB.AvgConnSen_conjuncts RB.AvgConnBl_contrasts
#> 1             0.09090909              0.07692308             0.09090909
#> 2             0.07692308              0.05555556             0.46153846
#> 3             0.00000000              0.00000000             0.17647059
#>   RB.AvgConnSen_contrasts RB.AvgConnBl_coordinating_conjuncts
#> 1              0.07692308                          0.09090909
#> 2              0.33333333                          0.15384615
#> 3              0.13636364                          0.17647059
#>   RB.AvgConnSen_coordinating_conjuncts RB.AvgConnBl_coordinating_connectives
#> 1                           0.07692308                             0.4545455
#> 2                           0.11111111                             1.1538462
#> 3                           0.13636364                             1.0000000
#>   RB.AvgConnSen_coordinating_connectives RB.AvgConnBl_disjunctions
#> 1                              0.3846154                 0.0000000
#> 2                              0.8333333                 0.3076923
#> 3                              0.7727273                 0.0000000
#>   RB.AvgConnSen_disjunctions RB.AvgConnBl_logical_connectors
#> 1                  0.0000000                       0.1818182
#> 2                  0.2222222                       0.8461538
#> 3                  0.0000000                       0.7058824
#>   RB.AvgConnSen_logical_connectors RB.AvgConnBl_oppositions
#> 1                        0.1538462               0.09090909
#> 2                        0.6111111               0.15384615
#> 3                        0.5454545               0.17647059
#>   RB.AvgConnSen_oppositions RB.AvgConnBl_order RB.AvgConnSen_order
#> 1                0.07692308         0.00000000          0.00000000
#> 2                0.11111111         0.07692308          0.05555556
#> 3                0.13636364         0.05882353          0.04545455
#>   RB.AvgConnBl_quasi_coordinators RB.AvgConnSen_quasi_coordinators
#> 1                      0.00000000                       0.00000000
#> 2                      0.07692308                       0.05555556
#> 3                      0.00000000                       0.00000000
#>   RB.AvgConnBl_reason_and_purpose RB.AvgConnSen_reason_and_purpose
#> 1                       0.3636364                        0.3076923
#> 2                       0.2307692                        0.1666667
#> 3                       0.3529412                        0.2727273
#>   RB.AvgConnBl_reference RB.AvgConnSen_reference RB.AvgConnBl_semi_coordinators
#> 1                      0                       0                     0.00000000
#> 2                      0                       0                     0.07692308
#> 3                      0                       0                     0.17647059
#>   RB.AvgConnSen_semi_coordinators RB.AvgConnBl_sentence_linking
#> 1                      0.00000000                      1.090909
#> 2                      0.05555556                      1.307692
#> 3                      0.13636364                      1.411765
#>   RB.AvgConnSen_sentence_linking RB.AvgConnBl_simple_subordinators
#> 1                      0.9230769                         0.5454545
#> 2                      0.9444444                         0.3076923
#> 3                      1.0909091                         0.2941176
#>   RB.AvgConnSen_simple_subordinators RB.AvgConnBl_temporal_connectors
#> 1                          0.4615385                        0.1818182
#> 2                          0.2222222                        0.1538462
#> 3                          0.2272727                        0.0000000
#>   RB.AvgConnSen_temporal_connectors RB.LexChainAvgSpan RB.LexChainMaxSp
#> 1                         0.1538462           1.642857                7
#> 2                         0.1111111           1.446809                7
#> 3                         0.0000000           1.557692                9
#>   RB.AvgLexChain RB.PercLexChainCoverage RB.AvgBlScore RB.AvgSenScore
#> 1     0.09090909              0.02173913      1.545407       1.390028
#> 2     0.07692308              0.01470588      1.724044       1.333472
#> 3     0.05882353              0.01234568      1.718639       1.402121
#>   RB.BlScoreStDev RB.SenScoreStDev RB.AvgBlAdjCoh_LeackockChodorow
#> 1       0.8274138        0.7325734                       0.3456678
#> 2       1.0229678        1.0678718                       0.3997900
#> 3       1.2547859        1.1039884                       0.4760863
#>   RB.AvgBlDocCoh_LeackockChodorow RB.AvgInterBlCoh_LeackockChodorow
#> 1                       0.7136200                         0.4461038
#> 2                       0.7296906                         0.4835340
#> 3                       0.7409131                         0.5873670
#>   RB.AvgIntraBlCoh_LeackockChodorow RB.AvgMidEndCoh_LeackockChodorow
#> 1                         0.4616434                        0.3023522
#> 2                         0.5045232                        0.4220118
#> 3                         0.3854458                        0.5137440
#>   RB.AvgSenAdjCoh_LeackockChodorow RB.AvgSenBlCoh_LeackockChodorow
#> 1                        0.2308217                       0.9647468
#> 2                        0.2018093                       0.9208567
#> 3                        0.3854458                       0.9426745
#>   RB.AvgStartMidCoh_LeackockChodorow RB.AvgTransCoh_LeackockChodorow
#> 1                          0.4165398                       0.3016517
#> 2                          0.3812699                       0.3089626
#> 3                          0.5056226                       0.3314135
#>   RB.AvgStartEndCoh_LeackockChodorow RB.DocFlAbsPosAcc_LeackockChodorow_MaxVal
#> 1                          0.9895363                                 0.6363636
#> 2                          0.5703230                                 0.3076923
#> 3                          0.7551799                                 0.5294118
#>   RB.DocFlAbsDistAcc_LeackockChodorow_MaxVal
#> 1                                  0.7272727
#> 2                                  1.5384615
#> 3                                  0.8235294
#>   RB.DocFlAdjAcc_LeackockChodorow_MaxVal RB.DocFlAvgCoh_LeackockChodorow_MaxVal
#> 1                               3.333333                              0.5627130
#> 2                               2.250000                              0.6200820
#> 3                               4.266667                              0.6555148
#>   RB.DocFlMaxOrdSeq_LeackockChodorow_MaxVal
#> 1                                 0.8181818
#> 2                                 0.6923077
#> 3                                 0.8823529
#>   RB.DocFlSpearmamCorr_LeackockChodorow_MaxVal
#> 1                                    0.9272727
#> 2                                    0.8461538
#> 3                                    0.9534314
#>   RB.DocFlAbsPosAcc_LeackockChodorow_AbvMeanStdev
#> 1                                       0.1818182
#> 2                                       0.1538462
#> 3                                       0.6470588
#>   RB.DocFlAbsDistAcc_LeackockChodorow_AbvMeanStdev
#> 1                                        2.1818182
#> 2                                        2.7692308
#> 3                                        0.9411765
#>   RB.DocFlAdjAcc_LeackockChodorow_AbvMeanStdev
#> 1                                     4.333333
#> 2                                     3.000000
#> 3                                     6.300000
#>   RB.DocFlAvgCoh_LeackockChodorow_AbvMeanStdev
#> 1                                    0.7123994
#> 2                                    0.6862546
#> 3                                    0.7442700
#>   RB.DocFlMaxOrdSeq_LeackockChodorow_AbvMeanStdev
#> 1                                       0.6363636
#> 2                                       0.6923077
#> 3                                       0.8235294
#>   RB.DocFlSpearmamCorr_LeackockChodorow_AbvMeanStdev RB.AvgBlAdjCoh_WuPalmer
#> 1                                          0.6636364               0.3323541
#> 2                                          0.5879121               0.3941398
#> 3                                          0.9240196               0.4104730
#>   RB.AvgBlDocCoh_WuPalmer RB.AvgInterBlCoh_WuPalmer RB.AvgIntraBlCoh_WuPalmer
#> 1               0.7018975                 0.4265859                 0.3477444
#> 2               0.7229328                 0.4797541                 0.4266026
#> 3               0.7178448                 0.5253572                 0.4428663
#>   RB.AvgMidEndCoh_WuPalmer RB.AvgSenAdjCoh_WuPalmer RB.AvgSenBlCoh_WuPalmer
#> 1                0.2480075                0.1738722               0.9622921
#> 2                0.3839423                0.1706410               0.9183316
#> 3                0.4505651                0.4428663               0.9451320
#>   RB.AvgStartMidCoh_WuPalmer RB.AvgTransCoh_WuPalmer RB.AvgStartEndCoh_WuPalmer
#> 1                  0.3480577               0.2955147                  0.9659091
#> 2                  0.3577486               0.2966081                  0.4459707
#> 3                  0.4364716               0.2980134                  0.6366632
#>   RB.DocFlAbsPosAcc_WuPalmer_MaxVal RB.DocFlAbsDistAcc_WuPalmer_MaxVal
#> 1                         0.4545455                          1.2727273
#> 2                         0.4615385                          0.9230769
#> 3                         0.7058824                          0.4705882
#>   RB.DocFlAdjAcc_WuPalmer_MaxVal RB.DocFlAvgCoh_WuPalmer_MaxVal
#> 1                       2.222222                      0.5204703
#> 2                       2.083333                      0.5548682
#> 3                       4.400000                      0.5734712
#>   RB.DocFlMaxOrdSeq_WuPalmer_MaxVal RB.DocFlSpearmamCorr_WuPalmer_MaxVal
#> 1                         0.7272727                            0.8363636
#> 2                         0.8461538                            0.9175824
#> 3                         0.9411765                            0.9754902
#>   RB.DocFlAbsPosAcc_WuPalmer_AbvMeanStdev
#> 1                               0.1818182
#> 2                               0.1538462
#> 3                               0.2941176
#>   RB.DocFlAbsDistAcc_WuPalmer_AbvMeanStdev RB.DocFlAdjAcc_WuPalmer_AbvMeanStdev
#> 1                                 2.727273                             2.600000
#> 2                                 2.000000                             3.000000
#> 3                                 1.882353                             6.388889
#>   RB.DocFlAvgCoh_WuPalmer_AbvMeanStdev RB.DocFlMaxOrdSeq_WuPalmer_AbvMeanStdev
#> 1                            0.6824084                               0.7272727
#> 2                            0.6333813                               0.6923077
#> 3                            0.6560723                               0.7058824
#>   RB.DocFlSpearmamCorr_WuPalmer_AbvMeanStdev RB.AvgBlAdjCoh_Path
#> 1                                  0.4363636           0.1540942
#> 2                                  0.7527473           0.1549885
#> 3                                  0.8651961           0.1357365
#>   RB.AvgBlDocCoh_Path RB.AvgInterBlCoh_Path RB.AvgIntraBlCoh_Path
#> 1           0.6128262             0.2490548            0.08035714
#> 2           0.6157296             0.2614884            0.11750000
#> 3           0.6060339             0.2549578            0.29416667
#>   RB.AvgMidEndCoh_Path RB.AvgSenAdjCoh_Path RB.AvgSenBlCoh_Path
#> 1           0.09330454           0.04017857           0.9563492
#> 2           0.12692848           0.04700000           0.9078449
#> 3           0.12782490           0.29416667           0.9358032
#>   RB.AvgStartMidCoh_Path RB.AvgTransCoh_Path RB.AvgStartEndCoh_Path
#> 1              0.1114012           0.1477113              0.9062500
#> 2              0.1243247           0.1019360              0.1282407
#> 3              0.1725397           0.1020735              0.1836065
#>   RB.DocFlAbsPosAcc_Path_MaxVal RB.DocFlAbsDistAcc_Path_MaxVal
#> 1                     0.4545455                      1.2727273
#> 2                     0.5384615                      0.9230769
#> 3                     0.4705882                      0.9411765
#>   RB.DocFlAdjAcc_Path_MaxVal RB.DocFlAvgCoh_Path_MaxVal
#> 1                   2.222222                  0.3102984
#> 2                   1.416667                  0.2836245
#> 3                   3.600000                  0.2859291
#>   RB.DocFlMaxOrdSeq_Path_MaxVal RB.DocFlSpearmamCorr_Path_MaxVal
#> 1                     0.7272727                        0.8363636
#> 2                     0.8461538                        0.9175824
#> 3                     0.8823529                        0.9411765
#>   RB.DocFlAbsPosAcc_Path_AbvMeanStdev RB.DocFlAbsDistAcc_Path_AbvMeanStdev
#> 1                           0.2727273                             2.545455
#> 2                           0.3076923                             2.153846
#> 3                           0.4117647                             1.647059
#>   RB.DocFlAdjAcc_Path_AbvMeanStdev RB.DocFlAvgCoh_Path_AbvMeanStdev
#> 1                         3.250000                        0.5402825
#> 2                         3.615385                        0.3950062
#> 3                         6.315789                        0.3656144
#>   RB.DocFlMaxOrdSeq_Path_AbvMeanStdev RB.DocFlSpearmamCorr_Path_AbvMeanStdev
#> 1                           0.7272727                              0.5181818
#> 2                           0.6153846                              0.6758242
#> 3                           0.7647059                              0.8431373
#>   RB.AvgBlAdjCoh_LSA RB.AvgBlDocCoh_LSA RB.AvgInterBlCoh_LSA
#> 1          0.2162169          0.4711404            0.3106523
#> 2          0.1233398          0.4489107            0.3133580
#> 3          0.1472582          0.4401688            0.2785260
#>   RB.AvgIntraBlCoh_LSA RB.AvgMidEndCoh_LSA RB.AvgSenAdjCoh_LSA
#> 1           0.20945084           0.1020611          0.20945084
#> 2           0.04617532           0.1621686          0.03694025
#> 3           0.32675691           0.1178623          0.26140553
#>   RB.AvgSenBlCoh_LSA RB.AvgStartMidCoh_LSA RB.AvgTransCoh_LSA
#> 1          0.9587929            0.14471317          0.2219136
#> 2          0.8676843            0.09533193          0.1215832
#> 3          0.9250621            0.21628388          0.1179168
#>   RB.AvgStartEndCoh_LSA RB.DocFlAbsPosAcc_LSA_MaxVal
#> 1             0.7327036                    0.4545455
#> 2             0.1005207                    0.6923077
#> 3             0.2079999                    1.0000000
#>   RB.DocFlAbsDistAcc_LSA_MaxVal RB.DocFlAdjAcc_LSA_MaxVal
#> 1                     1.4545455                  1.444444
#> 2                     0.4615385                  3.333333
#> 3                     0.0000000                  4.600000
#>   RB.DocFlAvgCoh_LSA_MaxVal RB.DocFlMaxOrdSeq_LSA_MaxVal
#> 1                 0.3794198                    0.6363636
#> 2                 0.3462775                    0.9230769
#> 3                 0.3238202                    1.0000000
#>   RB.DocFlSpearmamCorr_LSA_MaxVal RB.DocFlAbsPosAcc_LSA_AbvMeanStdev
#> 1                       0.7818182                          0.1818182
#> 2                       0.9670330                          0.3076923
#> 3                       1.0000000                          0.1176471
#>   RB.DocFlAbsDistAcc_LSA_AbvMeanStdev RB.DocFlAdjAcc_LSA_AbvMeanStdev
#> 1                            1.636364                           3.200
#> 2                            2.153846                           3.875
#> 3                            2.235294                           6.375
#>   RB.DocFlAvgCoh_LSA_AbvMeanStdev RB.DocFlMaxOrdSeq_LSA_AbvMeanStdev
#> 1                       0.5602619                          0.6363636
#> 2                       0.6377347                          0.6153846
#> 3                       0.4058362                          0.7058824
#>   RB.DocFlSpearmamCorr_LSA_AbvMeanStdev RB.AvgBlAdjCoh_LDA RB.AvgBlDocCoh_LDA
#> 1                             0.7909091          0.4697303          0.6283452
#> 2                             0.6978022          0.3126697          0.5808281
#> 3                             0.7818627          0.3808731          0.6231704
#>   RB.AvgInterBlCoh_LDA RB.AvgIntraBlCoh_LDA RB.AvgMidEndCoh_LDA
#> 1            0.5578005            0.4578321           0.2036443
#> 2            0.5184759            0.2126834           0.3410723
#> 3            0.5479840            0.5511218           0.2357932
#>   RB.AvgSenAdjCoh_LDA RB.AvgSenBlCoh_LDA RB.AvgStartMidCoh_LDA
#> 1           0.4578321          0.9686679             0.3266129
#> 2           0.2126834          0.8855748             0.2843673
#> 3           0.5511218          0.9516984             0.4852931
#>   RB.AvgTransCoh_LDA RB.AvgStartEndCoh_LDA RB.DocFlAbsPosAcc_LDA_MaxVal
#> 1          0.4690524             0.8367524                    0.3636364
#> 2          0.2595891             0.2886530                    0.6153846
#> 3          0.2987547             0.3439918                    0.4705882
#>   RB.DocFlAbsDistAcc_LDA_MaxVal RB.DocFlAdjAcc_LDA_MaxVal
#> 1                     1.4545455                  1.600000
#> 2                     0.6153846                  3.083333
#> 3                     0.8235294                  3.933333
#>   RB.DocFlAvgCoh_LDA_MaxVal RB.DocFlMaxOrdSeq_LDA_MaxVal
#> 1                 0.5947884                    0.7272727
#> 2                 0.5489599                    0.8461538
#> 3                 0.5805084                    0.8823529
#>   RB.DocFlSpearmamCorr_LDA_MaxVal RB.DocFlAbsPosAcc_LDA_AbvMeanStdev
#> 1                       0.8000000                         0.18181818
#> 2                       0.9560440                         0.23076923
#> 3                       0.9558824                         0.05882353
#>   RB.DocFlAbsDistAcc_LDA_AbvMeanStdev RB.DocFlAdjAcc_LDA_AbvMeanStdev
#> 1                            2.181818                        2.583333
#> 2                            2.000000                        3.266667
#> 3                            3.058824                        6.294118
#>   RB.DocFlAvgCoh_LDA_AbvMeanStdev RB.DocFlMaxOrdSeq_LDA_AbvMeanStdev
#> 1                       0.7071962                          0.6363636
#> 2                       0.7097034                          0.6923077
#> 3                       0.7304283                          0.6470588
#>   RB.DocFlSpearmamCorr_LDA_AbvMeanStdev RB.AvgBlAdjCoh_word2vec
#> 1                             0.6636364               0.2243804
#> 2                             0.7252747               0.2138584
#> 3                             0.6568627               0.2069513
#>   RB.AvgBlDocCoh_word2vec RB.AvgInterBlCoh_word2vec RB.AvgIntraBlCoh_word2vec
#> 1               0.4480521                 0.3093268                 0.2011581
#> 2               0.4704585                 0.3640401                 0.2764313
#> 3               0.4406463                 0.3125775                 0.2289906
#>   RB.AvgMidEndCoh_word2vec RB.AvgSenAdjCoh_word2vec RB.AvgSenBlCoh_word2vec
#> 1               0.06161313                0.2011581               0.9589169
#> 2               0.24614102                0.1658588               0.8918040
#> 3               0.21614282                0.2289906               0.9207015
#>   RB.AvgStartMidCoh_word2vec RB.AvgTransCoh_word2vec RB.AvgStartEndCoh_word2vec
#> 1                 0.07332757               0.2231703                  0.7912171
#> 2                 0.09096676               0.2282478                  0.2386241
#> 3                 0.12890010               0.1529742                  0.1983993
#>   RB.DocFlAbsPosAcc_word2vec_MaxVal RB.DocFlAbsDistAcc_word2vec_MaxVal
#> 1                         0.3636364                          1.0909091
#> 2                         0.3846154                          0.9230769
#> 3                         0.6470588                          0.9411765
#>   RB.DocFlAdjAcc_word2vec_MaxVal RB.DocFlAvgCoh_word2vec_MaxVal
#> 1                       2.111111                      0.3971140
#> 2                       2.818182                      0.4947970
#> 3                       3.400000                      0.3836729
#>   RB.DocFlMaxOrdSeq_word2vec_MaxVal RB.DocFlSpearmamCorr_word2vec_MaxVal
#> 1                         0.7272727                            0.8818182
#> 2                         0.7692308                            0.9285714
#> 3                         0.8823529                            0.9411765
#>   RB.DocFlAbsPosAcc_word2vec_AbvMeanStdev
#> 1                               0.3636364
#> 2                               0.4615385
#> 3                               0.5882353
#>   RB.DocFlAbsDistAcc_word2vec_AbvMeanStdev RB.DocFlAdjAcc_word2vec_AbvMeanStdev
#> 1                                1.4545455                             2.800000
#> 2                                0.9230769                             3.363636
#> 3                                0.7058824                             4.562500
#>   RB.DocFlAvgCoh_word2vec_AbvMeanStdev RB.DocFlMaxOrdSeq_word2vec_AbvMeanStdev
#> 1                            0.5274460                               0.8181818
#> 2                            0.5600191                               0.9230769
#> 3                            0.4670990                               0.7647059
#>   RB.DocFlSpearmamCorr_word2vec_AbvMeanStdev RB.AvgBlVoiceCoOcc
#> 1                                  0.7818182          0.5454545
#> 2                                  0.8846154          0.4615385
#> 3                                  0.9705882          0.3529412
#>   RB.AvgSenVoiceCoOcc RB.BlVoiceCoOccStDev RB.SenVoiceCoOccStDev
#> 1           0.4615385            0.5222330             0.5188745
#> 2           0.3333333            0.5188745             0.4850713
#> 3           0.3181818            0.4925922             0.4767313
#>   RB.AvgBlVoiceCumEff RB.AvgSenVoiceCumEff RB.BlVoiceCumEffStDev
#> 1           0.4307845            0.5148575             0.2682717
#> 2           0.4213276            0.3718415             0.2421134
#> 3           0.3786613            0.3681187             0.2988490
#>   RB.SenVoiceCumEffStDev RB.AvgBlVoiceMI RB.AvgSenVoiceMI RB.BlVoiceMIStDev
#> 1              0.6069549              -1               -1                -1
#> 2              0.5624504              -1               -1                -1
#> 3              0.5945355              -1               -1                -1
#>   RB.SenVoiceMIStDev RB.AvgVoice RB.AvgVoiceBlDist RB.AvgVoiceBlEnt
#> 1                 -1  0.09090909         0.6084679         1.767874
#> 2                 -1  0.07692308         0.5148575         1.767874
#> 3                 -1  0.05882353         0.4583388         1.738986
#>   RB.AvgVoiceReccBl RB.AvgVoiceReccSen RB.AvgVoiceSenDist RB.AvgVoiceSenEnt
#> 1         0.7142857           1.000000          0.5148575          1.767874
#> 2         1.0000000           1.714286          0.3718415          1.767874
#> 3         1.5714286           1.875000          0.3681187          1.899604
#>   RB.VoiceBlDistStDev RB.VoiceReccBlDistStDev RB.VoiceReccSenStDev
#> 1           0.5873037               1.0301575             1.414214
#> 2           0.5831434               0.7559289             1.030158
#> 3           0.6729940               1.9166297             2.471715
#>   RB.VoiceSenDistStDev RB.VoiceAvgSpan RB.VoiceMaxSpan RB.AvgSenSyll
#> 1            0.5831434               7               7      13.61538
#> 2            0.5466035               7               7      12.00000
#> 3            0.5808662               9               9      11.13636
#>   RB.AvgSenStressedSyll RB.AvgRhythmUnits RB.AvgRhythmUnitSyll
#> 1              3.769231          1.769231             7.695652
#> 2              3.555556          1.888889             6.617647
#> 3              3.727273          2.181818             5.354167
#>   RB.AvgRhythmUnitStreesSyll RB.LangRhythmCoeff RB.LangRhythmId RB.FrqRhythmId
#> 1                   2.130435          0.5797101               5     0.07692308
#> 2                   2.117647          0.5384615              10     0.05555556
#> 3                   1.875000          0.4098361               4     0.04545455
#>   RB.LangRhythmDiameter RB.SenAllit RB.SenAsson RB.AvgDepsBl_acl
#> 1                     9           0           0        0.3636364
#> 2                     6           0           0        0.0000000
#> 3                     6           0           1        0.1764706
#>   RB.AvgDepsSen_acl RB.AvgDepsBl_advcl RB.AvgDepsSen_advcl RB.AvgDepsBl_advmod
#> 1         0.3076923          0.1818182           0.1538462           0.6363636
#> 2         0.0000000          0.1538462           0.1111111           0.7692308
#> 3         0.1363636          0.1764706           0.1363636           0.7058824
#>   RB.AvgDepsSen_advmod RB.AvgDepsBl_amod RB.AvgDepsSen_amod RB.AvgDepsBl_appos
#> 1            0.5384615         0.4545455          0.3846154         0.09090909
#> 2            0.5555556         0.6153846          0.4444444         0.00000000
#> 3            0.5454545         0.7058824          0.5454545         0.05882353
#>   RB.AvgDepsSen_appos RB.AvgDepsBl_aux RB.AvgDepsSen_aux RB.AvgDepsBl_auxpass
#> 1          0.07692308        0.1818182         0.1538462            0.1818182
#> 2          0.00000000        0.6153846         0.4444444            0.1538462
#> 3          0.04545455        0.4117647         0.3181818            0.1176471
#>   RB.AvgDepsSen_auxpass RB.AvgDepsBl_case RB.AvgDepsSen_case RB.AvgDepsBl_cc
#> 1            0.15384615          1.454545          1.2307692       0.1818182
#> 2            0.11111111          1.461538          1.0555556       0.7692308
#> 3            0.09090909          1.117647          0.8636364       0.7058824
#>   RB.AvgDepsSen_cc RB.AvgDepsBl_ccomp RB.AvgDepsSen_ccomp RB.AvgDepsBl_compound
#> 1        0.1538462         0.09090909          0.07692308            0.09090909
#> 2        0.5555556         0.46153846          0.33333333            0.15384615
#> 3        0.5454545         0.11764706          0.09090909            0.11764706
#>   RB.AvgDepsSen_compound RB.AvgDepsBl_conj RB.AvgDepsSen_conj RB.AvgDepsBl_cop
#> 1             0.07692308         0.1818182          0.1538462        0.4545455
#> 2             0.11111111         0.6923077          0.5000000        0.1538462
#> 3             0.09090909         0.5882353          0.4545455        0.0000000
#>   RB.AvgDepsSen_cop RB.AvgDepsBl_csubj RB.AvgDepsSen_csubj
#> 1         0.3846154                  0                   0
#> 2         0.1111111                  0                   0
#> 3         0.0000000                  0                   0
#>   RB.AvgDepsBl_csubjpass RB.AvgDepsSen_csubjpass RB.AvgDepsBl_dep
#> 1                      0                       0        0.4545455
#> 2                      0                       0        0.5384615
#> 3                      0                       0        0.3529412
#>   RB.AvgDepsSen_dep RB.AvgDepsBl_det RB.AvgDepsSen_det RB.AvgDepsBl_discourse
#> 1         0.3846154        1.1818182         1.0000000                      0
#> 2         0.3888889        1.1538462         0.8333333                      0
#> 3         0.2727273        0.8235294         0.6363636                      0
#>   RB.AvgDepsSen_discourse RB.AvgDepsBl_dislocated RB.AvgDepsSen_dislocated
#> 1                       0                       0                        0
#> 2                       0                       0                        0
#> 3                       0                       0                        0
#>   RB.AvgDepsBl_dobj RB.AvgDepsSen_dobj RB.AvgDepsBl_expl RB.AvgDepsSen_expl
#> 1         0.6363636          0.5384615        0.09090909         0.07692308
#> 2         0.7692308          0.5555556        0.00000000         0.00000000
#> 3         0.8823529          0.6818182        0.05882353         0.04545455
#>   RB.AvgDepsBl_foreign RB.AvgDepsSen_foreign RB.AvgDepsBl_goeswith
#> 1                    0                     0                     0
#> 2                    0                     0                     0
#> 3                    0                     0                     0
#>   RB.AvgDepsSen_goeswith RB.AvgDepsBl_iobj RB.AvgDepsSen_iobj RB.AvgDepsBl_list
#> 1                      0                 0                  0                 0
#> 2                      0                 0                  0                 0
#> 3                      0                 0                  0                 0
#>   RB.AvgDepsSen_list RB.AvgDepsBl_mark RB.AvgDepsSen_mark RB.AvgDepsBl_mwe
#> 1                  0         0.6363636          0.5384615       0.09090909
#> 2                  0         0.4615385          0.3333333       0.00000000
#> 3                  0         0.3529412          0.2727273       0.05882353
#>   RB.AvgDepsSen_mwe RB.AvgDepsBl_name RB.AvgDepsSen_name RB.AvgDepsBl_neg
#> 1        0.07692308                 0                  0       0.18181818
#> 2        0.00000000                 0                  0       0.07692308
#> 3        0.04545455                 0                  0       0.11764706
#>   RB.AvgDepsSen_neg RB.AvgDepsBl_nmod RB.AvgDepsSen_nmod RB.AvgDepsBl_nsubj
#> 1        0.15384615         1.2727273          1.0769231          0.8181818
#> 2        0.05555556         1.0000000          0.7222222          1.3076923
#> 3        0.09090909         0.8823529          0.6818182          0.7058824
#>   RB.AvgDepsSen_nsubj RB.AvgDepsBl_nsubjpass RB.AvgDepsSen_nsubjpass
#> 1           0.6923077             0.09090909              0.07692308
#> 2           0.9444444             0.15384615              0.11111111
#> 3           0.5454545             0.23529412              0.18181818
#>   RB.AvgDepsBl_nummod RB.AvgDepsSen_nummod RB.AvgDepsBl_parataxis
#> 1           0.0000000           0.00000000             0.00000000
#> 2           0.0000000           0.00000000             0.07692308
#> 3           0.1176471           0.09090909             0.00000000
#>   RB.AvgDepsSen_parataxis RB.AvgDepsBl_punct RB.AvgDepsSen_punct
#> 1              0.00000000           1.454545            1.230769
#> 2              0.05555556           1.384615            1.000000
#> 3              0.00000000           1.705882            1.318182
#>   RB.AvgDepsBl_remnant RB.AvgDepsSen_remnant RB.AvgDepsBl_reparandum
#> 1                    0                     0                       0
#> 2                    0                     0                       0
#> 3                    0                     0                       0
#>   RB.AvgDepsSen_reparandum RB.AvgDepsBl_root RB.AvgDepsSen_root
#> 1                        0        0.09090909         0.07692308
#> 2                        0        0.00000000         0.00000000
#> 3                        0        0.23529412         0.18181818
#>   RB.AvgDepsBl_vocative RB.AvgDepsSen_vocative RB.AvgDepsBl_xcomp
#> 1                     0                      0          0.3636364
#> 2                     0                      0          0.1538462
#> 3                     0                      0          0.1176471
#>   RB.AvgDepsSen_xcomp
#> 1          0.30769231
#> 2          0.11111111
#> 3          0.09090909
```
