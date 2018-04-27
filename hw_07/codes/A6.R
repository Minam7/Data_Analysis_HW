accuracy_info = AccuracyCutoffInfo( train = train, test = test, 
                                    predict = "prediction", actual = "MannerOfDeath" )
accuracy_info$plot
accuracy_info$max_acc$accuracy

cat("maximum accuracy is:", accuracy_info$max_acc$accuracy, " with cutoff:", accuracy_info$max_acc$cutoff)
