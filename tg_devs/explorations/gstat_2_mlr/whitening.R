# colorRampPaletteAlpha
whiteAlpha <- function() {
  alpha <- seq(0,1,0.1)
  r <- col2rgb("white", alpha=T)
  r <- t(apply(r, 1, rep, length(alpha)))
  # Apply alpha
  r[4,] <- alpha*255
  r <- r/255.0
  codes <- (rgb(r[1,], r[2,], r[3,], r[4,]))
  return(codes)
}
