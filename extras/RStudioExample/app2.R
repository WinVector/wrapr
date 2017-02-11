
source("f.R")


library("wrapr")
saveDest <- 'bug.RDS'
f <- DebugFnW(saveDest, f)


inputs = c(4,5,2,9,0,8)

for(x in inputs) {
  f(x)
}

source("f.R")
p <- readRDS('bug.RDS')
do.call(p$fn_name, p$args)
