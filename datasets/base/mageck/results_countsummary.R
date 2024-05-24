Sweave("results_countsummary.Rnw");
library(tools);

texi2dvi("results_countsummary.tex",pdf=TRUE);

