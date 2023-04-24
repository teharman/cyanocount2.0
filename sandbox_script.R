library(EBImage)
library(tiff)
library(png)
library(pixmap)
library(raster)
library(av)

# clear environment and free unused memory
gc()

#Change directories here
savdir <- ("C:/Users/Tyler.Harman/Desktop/AI_Project/test_images/CSV/")
image_savdir <- ("C:/Users/Tyler.Harman/Desktop/AI_Project/test_images/convert_images")
video_savdir <- ("C:/Users/Tyler.Harman/Desktop/AI_Project/test_images/video_imgs")
videos <- list.files("C:/Users/Tyler.Harman/Desktop/AI_Project/test_images/test_video/"
                     , pattern = "", full.name = T)
video_names <- list.files("C:/Users/Tyler.Harman/Desktop/AI_Project/test_images/test_video/"
                           , pattern = "", full.name = F)

vidNames <- paste0(video_names)
vidIndex<-paste0(sub(".mp4", replacement = "", x=vidNames[[1]]))
vidpath<-file.path(video_savdir,vidIndex)
if(!dir.exists(vidpath)){
  dir.create(vidpath)
}
av_video_images(videos,destdir=vidpath,format="png")

images <- list.files(vidpath, pattern = "png", full.name = T)
images_names <- list.files(vidpath, pattern = "png", full.name = F)

imgNames <- paste0(images_names)
read_images <- lapply(images, readPNG)
img_transposed <- lapply(read_images,aperm,c(2,1,3))
names(images) <- imgNames

greyscale <- function(x, contrast = 2) {
  x <- contrast * x
  x <- x[, , 1] + x[, , 2] + x[, , 3]
  x <- x / max(x)
  x <- normalize(x, inputRange = c(0.1, 0.75))
  return(x)
}
grey_imgs<-lapply(img_transposed, greyscale, contrast = 0.2)
display(grey_imgs[[1]])
binary<-function(x, adj = 0.5) {
  binary_img<- x > adj
  return(binary_img)
}
img_neg<-function(x) {
  imgneg<-max(x)-x
  return(imgneg)
}
gamma_corr<-function(x) {
  gamma_img<-(0.2 + x)^3
  return(gamma_img)
}
lp_filter<-function(x,size=31,sigma=5) {
  w<-makeBrush(size=size,shape="gaussian",sigma = sigma)
  lp<-filter2(x,w)
  return(lp)
}
lp_imgs<-lapply(grey_imgs, lp_filter, size=51,sigma=1)
binary_img<-lapply(lp_imgs, binary, adj = 0.55)
neg_imgs<-lapply(binary_img, img_neg)
display(neg_imgs[[1]])
mapped <- function(x, threshold = 0.3) {
  x <- as.matrix(x)
  x[x < threshold] <- 0
  return(x)
}
imagesMapped <- lapply(neg_imgs, mapped, threshold = 0.2) #background intensity threshold adjustment


for (z in 1:length(images)) {
  Index<-paste0(sub(".png", replacement = "", x=imgNames[[z]]))
  Index1<-paste0(sub(".png", replacement = "/ ", x=imgNames[[z]]))
  newpath<-file.path(image_savdir,Index1)
  if(!dir.exists(newpath)){
    dir.create(newpath)
  }
  image <- thresh(imagesMapped[[z]], w = 17, h = 17, offset = 0.001)
  display(image)
  image1 <- fillHull(image)
  display(image1)
  image2 <- watershed(distmap(image1), tolerance = 0.5, ext = 1)
  nf<-computeFeatures.shape(image2)
  nr <- which(nf[, "s.area"] < 0)
  image3 <- rmObjects(image2, nr)
  display(image3)
  features.data<-computeFeatures(image3, grey_imgs[[z]])
  features<-as.data.frame(features.data)
  features<-cbind(features,frame_num=NA)
  st <- stackObjects(image3, imagesMapped[[z]])
  for(k in 1:dim(st)[3]) {
    st_img<-st[, , k]
    analyzed_image1<-paste0(sub(".png", replacement = " ", x=imgNames[[z]]),"_frame")
    analyzed_image2<-paste0(sub(".png", replacement = " ", x=analyzed_image1),k)
    analyzed_image3<-paste0(sub(".png", replacement = " ", x=analyzed_image2),"_analyzed.tiff")
    features$frame_num[k]<-cbind(k)
    writeImage(st_img,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
  }
  csv_save<-paste0(paste(Index,Sys.Date()),".csv")
  write.csv(features, paste0(savdir, csv_save)) #Change this CSV file name
}
