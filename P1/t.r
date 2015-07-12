d=read.table("hourly-male-4.ts",header=TRUE,sep=",")
require(data.table)
dt <- data.table(d)
dt[, .SD[any(ts!=0)], by = time]