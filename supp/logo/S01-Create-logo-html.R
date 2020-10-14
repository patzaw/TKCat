make_path <- function(x, y, scale, transx=NA, transy=NA){
   x <- x*scale
   y <- (-y)*scale
   if(is.na(transx)){
      transx <- min(x)
   }
   if(is.na(transy)){
      transy <- min(y)
   }
   x <- x-transx+lwd
   y <- y-transy+lwd
   i <- 1
   pts <- paste0('M', x[i], ',', y[i])
   for(i in 2:length(x)){
      pts <- c(pts, paste0('L', x[i], ',', y[i]))
   }
   attr(pts, "trans") <- c(transx, transy)
   return(pts)
}

scale <- 8

fill <- "saddlebrown" #"#8A4513"
fill2 <- "burlywood" # "#DEB887"
lcol <- "black" #"#000000"
bg <- "white" #"#FFFFFF"
lwd <- 2*scale

## SVG ----
tw <- sprintf(
   '<svg id="logo" width="%s" height="%s">',
   (30*2*scale)+(lwd*2), (30*2*scale)+(lwd*2)
)

## Hexagon ----
alpha <- c(seq(1, 11, by=2), 1)*pi/6
x <- 0+cos(alpha)*30
y <- 3+sin(alpha)*30
pts <- make_path(x, y, scale)
transx <- attr(pts, "trans")[1]
transy <- attr(pts, "trans")[2]
pts <- c(pts, "Z")
pts <- paste(pts, collapse=" ")
path <- sprintf(
   '<path d="%s" stroke="%s" stroke-width="%s" fill="%s" stroke-linecap="round"/>',
   pts, fill, lwd, bg
)
tw <- c(tw, path)

# ## Title ----
x <- 0
y <- 15
x <- x*scale
y <- (-y)*scale
x <- x-transx+lwd
y <- y-transy+lwd
path <- sprintf(
   paste0(
      '<text x="%s" y="%s" ',
      'text-anchor="middle" fill="%s" ',
      'stroke="%s"',
      'font-size="%s" ',
      'font-family="Geneva"',
      '>',
      'TKCat',
      '</text>'
   ),
   x, y, fill,
   fill,
   lwd*4
)
tw <- c(tw, path)

## Acorns ----
get_acorn_xy <- function(
   posx=0, posy=0,
   r=5.5, cx=2, rr=0.95, r4r=1.2
){
# posx <- 0
# posy <- 0
# r <- 4
# cx <- 2
   c1 <- c(posx-cx, posy)
   c2 <- c(posx+cx, posy)
   
   alpha0 <- acos(cx/r)
   
   alpha1 <- seq(2*pi-alpha0, 2*pi, length.out=30)
   x1 <- r*cos(alpha1) + c1[1]
   y1 <- r*sin(alpha1) + c1[2]
   alpha2 <- seq(pi, pi+alpha0, length.out=30)
   x2 <- r*cos(alpha2) + c2[1]
   y2 <- r*sin(alpha2) + c2[2]
   
   
   r3 <- abs((r-cx)/cos(pi+alpha0))
   c3 <- c(posx, r3*sin(pi+alpha0)+posy)
   alpha3 <- seq(alpha0, pi-alpha0, length.out=30)
   x3 <- r3*cos(alpha3) + c3[1]
   y3 <- r3*sin(alpha3) + c3[2]
   
   # rr <- 0.95
   r4 <- abs(cx-r)/rr
   # r4r <- 1.2
   # alpha00 <- pi+alpha0
   # r4*cos(alpha00) <- (cx-r)
   alpha00 <- acos((cx-r)/r4)
   alpha00 <- alpha00+(pi)
   c4 <- c(posx, posy-(r4*sin(alpha00)/r4r))
   alpha4 <- seq(alpha00, 5*pi-alpha00, length.out=100)
   x4 <- r4*cos(alpha4) + c4[1]
   y4 <- r4*sin(alpha4)/r4r + c4[2]
   
   x5 <- c4[1] + c(0, cos(6*pi/16)*r4*1.1, cos(7*pi/16)*r4*1.2, 0)
   y5 <- c4[2] + c(r4r/2, sin(6*pi/16)*r4*1.1, sin(7*pi/16)*r4*1.2, r4r/2)
   return(list(
      body=list(x=c(x1, x3, x2), y=c(y1, y3, y2)),
      cap=list(x=x4, y=y4),
      stem=list(x=x5, y=y5)
   ))
}
get_acorn_svg <- function(apos){
   toRet <- c()
   pts <- make_path(apos$body$x, apos$body$y, scale, transx, transy)
   pts <- c(pts, "Z")
   pts <- paste(pts, collapse=" ")
   path <- sprintf(
      '<path d="%s" stroke="%s" stroke-width="%s" fill="%s" stroke-linejoin="round" />',
      pts, lcol, lwd/4, fill2
   )
   toRet <- c(toRet, path)
   pts <- make_path(apos$cap$x, apos$cap$y, scale, transx, transy)
   pts <- c(pts, "Z")
   pts <- paste(pts, collapse=" ")
   path <- sprintf(
      '<path d="%s" stroke="%s" stroke-width="%s" fill="%s" stroke-linejoin="round" />',
      pts, lcol, lwd/4, fill
   )
   toRet <- c(toRet, path)
   pts <- make_path(apos$stem$x, apos$stem$y, scale, transx, transy)
   pts <- c(pts, "Z")
   pts <- paste(pts, collapse=" ")
   path <- sprintf(
      '<path d="%s" stroke="%s" stroke-width="%s" fill="%s" stroke-linejoin="round" />',
      pts, lcol, lwd/4, lcol
   )
   toRet <- c(toRet, path)
   return(toRet)
}

for(i in 0){
   tw <- c(tw, get_acorn_svg(get_acorn_xy(0+9*i, -20)))
}
for(i in 0:3){
   tw <- c(tw, get_acorn_svg(get_acorn_xy(-13.5+9*i, -11)))
}
for(i in 0:4){
   tw <- c(tw, get_acorn_svg(get_acorn_xy(-18+9*i, -2)))
}
for(i in 0:3){
   tw <- c(tw, get_acorn_svg(get_acorn_xy(-13.5+9*i, 7)))
}

## SVG ----
tw <- c(tw, '</svg>')
doc <- htmltools::htmlTemplate(
   "supp/logo/TKCat-template.html", svg=htmltools::HTML(tw)
)
write(as.character(doc), ncolumns=1, file="supp/logo/TKCat.html")
