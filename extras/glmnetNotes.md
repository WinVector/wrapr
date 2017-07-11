    library("glmnet")

    ## Loading required package: Matrix

    ## Loading required package: foreach

    ## Loaded glmnet 2.0-10

    packageVersion("glmnet")

    ## [1] '2.0.10'

    # Observation 1: glmnet lower.limits does not constrain intercept term.
    d1 <- data.frame(y= c(-10,-100,-1000), 
                     x=c(1,2,3), 
                     x2=c(0,1,5))
    m1 <- glmnet(as.matrix(d1[, c('x', 'x2'), drop = FALSE]), 
                 d1$y, 
                 lower.limits = 0, 
                 alpha=0.0, 
                 lambda=c(0, 1.0e-5, 1.0e-3, 0.1, 1, 10),
                 intercept = TRUE, 
                 family = 'gaussian')
    coef(m1, s=0)

    ## 3 x 1 sparse Matrix of class "dgCMatrix"
    ##                1
    ## (Intercept) -370
    ## x              0
    ## x2             .

    as.numeric(coef(m1, s=0))

    ## [1] -370    0    0

    # Observation 2: gmlnet does not accept constant columns (such as "c").
    d2 <- data.frame(y= c(1,2,3),
                     c= c(1,1,1),
                     x= c(0,1,0),
                     x2= c(0,0,1))
    l2 <- lm(y ~ 0+ c + x + x2, data = d2)
    coefficients(l2)

    ##  c  x x2 
    ##  1  1  2

    predict(l2, newdata = d2)

    ## 1 2 3 
    ## 1 2 3

    m2 <- glmnet(as.matrix(d2[, c('c', 'x', 'x2'), drop = FALSE]), 
                 d2$y, 
                 alpha=0.0, 
                 lambda=c(0, 1.0e-5, 1.0e-3, 0.1, 1, 10),
                 intercept = FALSE, 
                 family = 'gaussian')
    coef(m2, s=0)

    ## 4 x 1 sparse Matrix of class "dgCMatrix"
    ##             1
    ## (Intercept) .
    ## c           .
    ## x           2
    ## x2          3

    as.numeric(coef(m2, s=0))

    ## [1] 0 0 2 3

    predict(m2, s=0, newx=as.matrix(d2[, c('c', 'x', 'x2')]))

    ##      1
    ## [1,] 0
    ## [2,] 2
    ## [3,] 3

    # Observation 3: glmnet does not always match lm()
    # I think it is some implicit regularization in non-intercept
    # terms (or convergence), and intercept is special.
    # also likely important to center/scale variables for glmnet
    # (which we did not do).
    d3 <- readRDS(file='d3.RDS')
    l3 <- lm(y ~ x + x2, data = d3)
    summary(l3)

    ## 
    ## Call:
    ## lm(formula = y ~ x + x2, data = d3)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ##   -527594    -53645    -12512     -4933 520561169 
    ## 
    ## Coefficients:
    ##              Estimate Std. Error t value Pr(>|t|)
    ## (Intercept) 3.212e+03  6.296e+04   0.051    0.959
    ## x           5.411e+02  6.582e+02   0.822    0.411
    ## x2          1.857e-02  6.833e-01   0.027    0.978
    ## 
    ## Residual standard error: 5401000 on 11997 degrees of freedom
    ## Multiple R-squared:  0.0008851,  Adjusted R-squared:  0.0007186 
    ## F-statistic: 5.314 on 2 and 11997 DF,  p-value: 0.004933

    m3 <- glmnet(as.matrix(d3[, c('x', 'x2'), drop = FALSE]), 
                 d3$y, 
                 lower.limits = 0, 
                 alpha=0.0, 
                 lambda=c(0, 1.0e-5, 1.0e-3, 0.1, 1, 10),
                 intercept = TRUE, 
                 family = 'gaussian')
    coef(m3, s=0)

    ## 3 x 1 sparse Matrix of class "dgCMatrix"
    ##                       1
    ## (Intercept) 2826.856891
    ## x            548.570231
    ## x2             0.011105

    m3b <- glmnet(as.matrix(d3[, c('x', 'x2'), drop = FALSE]), 
                 d3$y, 
                 alpha=0.0, 
                 lambda=c(0, 1.0e-5, 1.0e-3, 0.1, 1, 10),
                 intercept = TRUE, 
                 family = 'gaussian')
    coef(m3b, s=0)

    ## 3 x 1 sparse Matrix of class "dgCMatrix"
    ##                       1
    ## (Intercept) 2826.856891
    ## x            548.570231
    ## x2             0.011105
