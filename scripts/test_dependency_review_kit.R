# Synthetic-only tests: no enclave results used.
source("inst/enclave/dependency-review-v1/review.R")
passed <- 0L
check <- function(x) {stopifnot(isTRUE(x));passed <<- passed+1L}
bad <- function(expr) inherits(tryCatch({force(expr);NULL},error=identity),"error")
z <- list(round_id="react1.r01",status="fitted_pending_review",limitations="test <escaped>",
  fit=list(validation=data.frame(n=1000,positive=100,observed=.1,mean_prediction=.1,
    brier=.08,log_loss=.25,baseline_log_loss=.33),
    calibration=data.frame(field=c("all","age_band"),level=c("all","<test&group>"),
      n=c(1000,500),positive=c(100,50),expected_positive=c(100,50)),
    model=list(coefficients=c(`(Intercept)`=-2,age=.2)),diagnostics=list("fictional")))
check(length(dr_assess(z)$flags)==0)
y <- z;y$fit$validation$log_loss <- .4
check(any(grepl("baseline",dr_assess(y)$flags)))
y <- z;y$fit$validation$mean_prediction <- .2
check(any(grepl("Overall",dr_assess(y)$flags)))
y <- z;y$fit$calibration$expected_positive[2] <- 100
check(any(grepl("supported groups",dr_assess(y)$flags)))
y <- z;y$fit$calibration$positive[2] <- 1
check(any(grepl("small",dr_assess(y)$flags)))
y <- z;y$fit$model$coefficients[2] <- 6
check(any(grepl("Large",dr_assess(y)$flags)))
y <- z;y$fit$diagnostics <- NULL
check(any(grepl("absent",dr_assess(y)$flags)))
y <- z;y$fit$validation$brier <- NA_real_
check(any(grepl("non-finite",dr_assess(y)$flags)))
y <- z;y$status <- "refit_failed";y$fit$model <- NULL
check(any(grepl("Fit status",dr_assess(y)$flags)))
check(grepl("&lt;test&amp;group&gt;",dr_plot(z$fit$calibration),fixed=TRUE))
d <- tempfile("fictional-review-");dir.create(d)
rounds <- c(sprintf("react1.r%02d",1:19),sprintf("react2.r%02d",1:6))
m <- setNames(lapply(rounds,function(r){v<-z;v$round_id<-r;v}),rounds)
p <- file.path(d,"parameters-AND-COUNTS-NOT-APPROVED.rds");saveRDS(m,p)
before <- tools::md5sum(p)
out <- file.path(d,"review.html");s <- react_dependency_review(d,out)
check(nrow(s)==25L && all(s$Review=="No automated flags"))
check(identical(before,tools::md5sum(p)))
h <- paste(readLines(out),collapse="\n")
check(grepl("Keep inside the enclave",h,fixed=TRUE))
check(!grepl("https?://|<script|<img",h))
check(length(gregexpr("<svg ",h,fixed=TRUE)[[1]])==50L)
check(bad(react_dependency_review(d,out)))
m[[1]] <- NULL;saveRDS(m,p)
s <- react_dependency_review(d,file.path(d,"missing.html"))
check(s$Review[1]=="MISSING ROUND")
m[[1]]$round_id <- "wrong";saveRDS(m,p)
check(bad(react_dependency_review(d,file.path(d,"bad.html"))))
preview <- Sys.getenv("DEPENDENCY_REVIEW_PREVIEW")
if (nzchar(preview)) {if(file.exists(preview)) stop("Preview exists");stopifnot(file.copy(out,preview))}
cat(passed,"checks passed. Fictional preview:",out,"\n")
