#load image
library(png) 

img <- readPNG( "image.png")

#safe orginal size
orginal_size <- length(img)

#split RGB colors
r <- img[,,1]
g <- img[,,2]
b <- img[,,3]

#I decided to PCA each color separately
#I will make compresed image biger but it wil be easier to show
img.r.pca <- prcomp(r, center = FALSE)
img.g.pca <- prcomp(g, center = FALSE)
img.b.pca <- prcomp(b, center = FALSE)

rgb.pca <- list(img.r.pca, img.g.pca, img.b.pca)

compressed_size <- rep(0,8)

for( i in 0:7){
  #uncompress image
  pca.img <- sapply(rgb.pca, function(j) {
    compressed.img <- j$x[,1:2**i] %*% t(j$rotation[,1:2**i])
  }, simplify = 'array')
  
  #count compressed image size (numbers of numbers used to uncompress image)
  compressed_size[i+1] <- sum( 
    sapply(rgb.pca, function(j) {
      length(j$x[,1:2**i]) + length(j$rotation[,1:2**i])
    }, simplify = 'array')
  )
  
  #safe image
  writeJPEG( pca.img, paste0('img_compressed_', 2**i, '.jpg'))
}

#how many times image was compresed
orginal_size / compressed_size

#1            2         4          8           16         32         64         128    
#159.938744  79.969372  39.984686  19.992343   9.996172   4.998086   2.499043   1.249521

#Summary
#the best compression is, of course, using only first PC, it's using 160 less space, but the image is lost
#using first 128 PC give near the excellent image but uses only 80% space of the original image
#in my opinion one should use the first 32 PC, as one could still read text on the board and safe 5 times less space