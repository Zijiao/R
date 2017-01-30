
# Cutting analysis, not used now but may be helpful in future

heights = cluster$height

# baseIndex = floor(0.75 * length(heights))
# heights <- heights[floor(0.75 * length(heights)):length(heights)]

med <- median(heights)

subtractMedian <- function(value) {
  return(abs(value - med))
}

MAD <- median(sapply(heights, subtractMedian))

mzscore <- function(value) {
  m <- 0.6745 * (value - med) / MAD
  return(abs(m))
}

Mzscore <- sapply(heights, mzscore)

outlierIndicators <- Mzscore > 3.5

outliers <- which(outlierIndicators == TRUE)
# if uses only last 25% heights, add baseIndex to each index


