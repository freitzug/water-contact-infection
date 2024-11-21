

#slightly modified version of the CreateTableOne2 function from the jstable (version 1.1.1) package

CreateTableOne2 <- function (data, strata, vars, factorVars, includeNA = F, test = T,
          testApprox = chisq.test, argsApprox = list(correct = TRUE),
          testExact = fisher.test, argsExact = list(workspace = 2 *
                                                      10^5), testNormal = oneway.test, argsNormal = list(var.equal = F),
          testNonNormal = kruskal.test, argsNonNormal = list(NULL),
          showAllLevels = T, printToggle = F, quote = F, smd = F, Labels = F,
          exact = NULL, nonnormal = NULL, catDigits = 1, contDigits = 2,
          pDigits = 3, labeldata = NULL, minMax = F, showpm = T, addOverall = F)
{
  setkey <- variable <- level <- . <- val_label <- NULL
  if (length(strata) != 1) {
    stop("Please select only 1 strata")
  }
  vars.ex <- 0
  if (length(vars.ex) > 0) {
    warning("Variables other than numeric or factor types are excluded.")
    vars <- setdiff(vars, vars.ex)
  }
  res <- tableone::CreateTableOne(vars = vars, strata = strata,
                                  data = data, factorVars = factorVars, includeNA = includeNA,
                                  test = test, testApprox = testApprox, argsApprox = argsApprox,
                                  testExact = testExact, argsExact = argsExact, testNormal = testNormal,
                                  argsNormal = argsNormal, testNonNormal = testNonNormal,
                                  argsNonNormal = argsNonNormal, smd = smd, addOverall = addOverall)
  factor_vars <- res[["MetaData"]][["varFactors"]]
  if (Labels & !is.null(labeldata)) {
    labelled::var_label(data) <- sapply(names(data), function(v) {
      as.character(labeldata[get("variable") == v, "var_label"][1])
    }, simplify = F)
    data.table::setkey(labeldata, variable, level)
    res0 <- tableone::CreateTableOne(vars = vars, data = data,
                                     factorVars = factorVars, includeNA = includeNA)
    for (i in seq_along(res0$CatTable)) {
      for (j in factor_vars) {
        lvs <- res0$CatTable[[i]][[j]]$level
        res0$CatTable[[i]][[j]]$level <- labeldata[.(j,
                                                     lvs), val_label]
      }
    }
    ptb1.res0 <- print(res0, showAllLevels = showAllLevels,
                       printToggle = printToggle, quote = quote, varLabels = Labels,
                       nonnormal = nonnormal, catDigits = catDigits, contDigits = contDigits,
                       minMax = minMax)
    ptb1.rn <- rownames(ptb1.res0)
    ptb1.rn <- gsub("(mean (SD))", "", ptb1.rn, fixed = T)
  }
  vars.fisher <- sapply(factor_vars, function(x) {
    is(tryCatch(chisq.test(table(data[[strata]], data[[x]])),
                error = function(e) e, warning = function(w) w),
       "warning")
  })
  vars.fisher <- factor_vars[unlist(vars.fisher)]
  if (is.null(exact) & length(vars.fisher) > 0) {
    exact <- vars.fisher
  }
  ptb1 <- print(res, showAllLevels = showAllLevels, printToggle = printToggle,
                quote = quote, smd = smd, varLabels = Labels, nonnormal = nonnormal,
                exact = exact, catDigits = catDigits, contDigits = contDigits,
                pDigits = pDigits, minMax = minMax)
  if (showpm) {
    ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ] <- gsub("\\(",
                                                                 "± ", ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)),
                                                                 ])
    ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)), ] <- gsub("\\)",
                                                                 "", ptb1[grepl("\\(mean \\(SD\\)\\)", rownames(ptb1)),
                                                                 ])
  }
  rownames(ptb1) <- gsub("(mean (SD))", "", rownames(ptb1),
                         fixed = T)
  if (Labels & !is.null(labeldata)) {
    rownames(ptb1) <- ptb1.rn
    if (showAllLevels == T)
      ptb1[, 1] <- ptb1.res0[, 1]
  }
  if (Labels & !is.null(labeldata)) {
    colname.group_var <- unlist(labeldata[.(strata, names(res$CatTable)),
                                          val_label])
    if (is.na(colname.group_var[1]) & addOverall) {
      colname.group_var[1] <- "Overall"
    }
    if (showAllLevels == T) {
      colnames(ptb1)[1:(length(colname.group_var) + 1)] <- unlist(c(labeldata[get("variable") ==
                                                                                strata, "var_label"][1], colname.group_var))
    }
    else {
      colnames(ptb1)[1:length(colname.group_var)] <- colname.group_var
    }
  }
  sig <- ifelse(ptb1[, "p"] == "<0.001", "0", ptb1[, "p"])
  sig <- as.numeric(as.vector(sig))
  sig <- ifelse(sig <= 0.05, "**", "")
  ptb1 <- cbind(ptb1, sig)
  return(ptb1)
}
