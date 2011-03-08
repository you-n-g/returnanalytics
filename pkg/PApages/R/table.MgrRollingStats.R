`table.MgrRollingStats` <- function (R, periods = subset(c(length(na.omit(as.matrix(R[,1]))), 12, 36, 60), c(length(na.omit(as.matrix(R[,1]))), 12, 36, 60) <= length(na.omit(as.matrix(R[, 1])))), FUNCS = c("mean", "sd"), funcs.names = c("Mean", "Std Dev"), digits = 4, ...)
{
    R = checkData(R[,1,drop=FALSE], method = "zoo")
    freq = periodicity(R)
    switch(freq$scale, minute = {
        freq.lab = "minute"
    }, hourly = {
        freq.lab = "hour"
    }, daily = {
        freq.lab = "day"
    }, weekly = {
        freq.lab = "week"
    }, monthly = {
        freq.lab = "month"
    }, quarterly = {
        freq.lab = "quarter"
    }, yearly = {
        freq.lab = "year"
    })
    if (length(FUNCS) != length(funcs.names)) {
        warning("The length of the names vector is unequal to the length of the functions vector, so using FUNCS for naming.")
        funcs.names = NA
    }
    if (is.na(funcs.names[1]))
        funcs.names = FUNCS

    for (FUNC in FUNCS) {
        row.values = vector("numeric", 0)
        func.name = funcs.names[grep(FUNC, FUNCS)]
        for (period in periods) {
            row.values = c(row.values, apply(as.matrix(last(R, n = period)),
              FUN = FUNC, ..., MARGIN = 2))
        }
        row.values = as.matrix(row.values, nrow=1)
        if (FUNC == FUNCS[1]) {
            result = data.frame(Value = t(row.values))
        }
        else {
            nextrow = data.frame(Value = t(row.values))
            result = rbind(result, nextrow)
        }
    }
    columnnames = vector("character", 0)
    for(i in 1:length(periods)) 
        columnnames[i] = paste(periods[i],freq.lab)
    colnames(result) = columnnames
    rownames(result) = funcs.names
    ans = base::round(result, digits)
    ans
}