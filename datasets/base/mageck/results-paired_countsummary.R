Sweave("results-paired_countsummary.Rnw");
library(tools);

texi2dvi("results-paired_countsummary.tex",pdf=TRUE);

