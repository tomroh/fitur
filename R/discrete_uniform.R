#
# dunifdisc<-function(x, min=0, max=1) {
#   ifelse(x>=min & x<=max & round(x)==x, 1/(max-min+1), 0)
# }
# punifdisc<-function(q, min=0, max=1) {
#   ifelse(q<min, 0, ifelse(q>=max, 1, (floor(q)-min+1)/(max-min+1)))
# }
# qunifdisc<-function(p, min=0, max=1) {
#   floor(p*(max-min+1))
# }
# runifdisc<-function(n, min=0, max=1) {
#   sample(min:max, n, replace=T)
# }
#
#
# fit_uniform_discrete <- function(x) {
#   stopifnot(is.integer(x))
#   x <- sort(x)
#   values <- unique(x)
#   # convert to factors so tabulate doesn't ignore numbers <= 0
#   probs <- tabulate(as.factor(x))/length(x)
#
#   d <- function(x) {
#     probs[x == values]
#   }
#
#   p <- function(q) {
#     sum(probs[q <= values])
#   }
#
#   q <- function(p) {
#     max(values[cumsum(probs) <= p])
#   }
#
#   r <- function(n) {
#     sample(x = values, size = n, prob = probs, replace = TRUE)
#   }
#   list(d = d, p = p, q = q, r= r)
#
# }
