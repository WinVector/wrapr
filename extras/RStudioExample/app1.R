
source("f.R")


options(error = quote(dump.frames("testdump", TRUE)))


inputs = c(4,5,2,9,0,8)

for(x in inputs) {
  f(x)
}


load("testdump.rda")
debugger(testdump)
