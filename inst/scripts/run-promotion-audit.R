#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2L) stop("Usage: run-promotion-audit.R <evidence.rds> <report.md>")
library(eyeprocess)
evidence <- readRDS(args[1L])
audit <- audit_model_promotion(evidence)
write_model_promotion_report(audit, args[2L])
print(audit)
if (any(audit$models$status != "promotable")) quit(status = 2L)
