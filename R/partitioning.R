##################
# Data partitions

frm <- function(names) {
    reformulate(names[!names %in% c('CaseId', 'CaseWeight', 'fatal')], 'fatal')
}

part_whole <- function(df, prop=1, n=NULL) {
    if (!is.null(n))
        prop <- (n / nrow(df))
    repl <- ifelse(prop <= 1, FALSE, TRUE)
    df[sample(nrow(df), floor(nrow(df) * prop)), ]
}

part_split <- function(df, prop.test=0.3, ...) {
    ddf <- part_whole(df, ...)
    n <- nrow(ddf)
    list(
        ddf[1:round((1-prop.test) * n), ], # train
        ddf[(round((1-prop.test) * n) + 1):n, ] # test
    )
}

part_balanced <- function(df, response, response.min, response.maj) {
    # split by response class to keep this variable stratified
    df_minority <- df[df[response] == response.min, ]
    df_majority <- df[df[response] == response.maj, ]
    # split test/train quantities from both classes
    df_minority_split <- part_split(df_minority)
    df_train_min <- df_minority_split[[1]]
    df_test_min <- df_minority_split[[2]]
    df_majority_split <- part_split(df_majority)
    df_train_maj <- df_majority_split[[1]]
    df_test_maj <- df_majority_split[[2]]
    # balance the training 
    p <- 0.5 # 0 = shrink maj to size of min, 1 = expand min to size of maj
    n <- floor(nrow(df_train_min) + p * (nrow(df_train_maj) - nrow(df_train_min)))
    df_train_min <- df_train_min[sample(nrow(df_train_min), n, replace=TRUE), ] ## amend to make sure it includes everything at least once
    df_train_maj <- df_train_maj[sample(nrow(df_train_maj), n), ]
    df_train <- rbind(df_train_min, df_train_maj)
    # no need to balance test
    df_test <- rbind(df_test_min, df_test_maj)
    # shuffle
    df_train <- df_train[sample(nrow(df_train)), ]
    df_test <- df_test[sample(nrow(df_test)), ]
    # return train and test
    list(df_train, df_test)
}
part_balanced(df, "fatal", 0, 1)

#ff <- frm(names(df)[6:])
full <- lm(ff, data=df)
null <- lm(fatal ~ 1, data=df)
stepAIC(null, scope=list(lower=null, upper=full), direction="forward", trace=FALSE)

vars <- names(df)[!names(df) %in% c('CaseId', 'CaseWeight', 'fatal')]
#for (i in 1:(length(vars)-3)) {
    ff <- frm(vars[(i+51):length(i)])
    #ff <- frm(vars)
    full <- lm(ff, data=df_train)
    null <- lm(fatal ~ 1, data=df_train)
    print(stepAIC(null, scope=list(lower=null, upper=full), direction="forward", trace=FALSE)$call)
#}

for (var in vars) {
    print(chisq.test(df[var], df$fatal))
}
