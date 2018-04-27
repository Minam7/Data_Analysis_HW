cost_fp = 100
cost_fn = 200
roc_info = ROCInfo( data = cm_info$data, predict = "predict", 
                    actual = "actual", cost.fp = cost_fp, cost.fn = cost_fn )
plot(roc_info$plot)
cat("optimal cutoff value:", roc_info$cutoff, "and the corresponding true positive rate:",  roc_info$sensitivity, "and false positive rate:", 1 - roc_info$specificity)
