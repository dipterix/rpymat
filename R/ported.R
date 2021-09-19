fastqueue2 <- function (init = 20L, missing_default = NULL) {
  queue <- fastmap::fastqueue(init = init, missing_default = missing_default)
  head <- 0
  count <- 0
  i <- NA
  ev <- new.env(parent = environment(.subset2(queue, "as_list")))
  q <- NULL

  queue$at <- with(ev, {
    function(i){
      if (is.na(i) || i < 1L || i > count) {
        stop("subscript out of bounds")
      }
      q[[head - count + i]]
    }
  })


  queue$mat <- with(ev, {
    function(i){
      q[head - count + i]
    }
  })
  class(queue) <- c("fastqueue2", "list")
  queue
}
