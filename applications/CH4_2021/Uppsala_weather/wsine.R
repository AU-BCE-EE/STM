# Sine function for weather

wsine <- function(doy, min, max, amp = max - min, maxdoy = 170) {

  w <- min + amp * 0.5 *  (sin((days - maxdoy)/365 * 2 *pi + 0.5 * pi) + 1)

  return(w)

}

