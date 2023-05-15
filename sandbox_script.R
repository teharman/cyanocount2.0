library(EBImage)
library(tiff)
library(png)
library(pixmap)
library(raster)
library(magick)
library(cellcount)
library(dplyr)
library(jjb)

# clear environment and free unused memory
gc()

#Change directories here
img_dir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/version_2/5_4_demo/NZ_Anabena/40x_AccuScope/")
images <- list.files(img_dir, pattern = "tif", full.name = T)
images_names <- list.files(img_dir, pattern = "tif", full.name = F)

imgNames <- paste0(images_names)
read_images <- lapply(images, readTIFF)
img_transposed <- lapply(read_images,aperm,c(2,1,3))
names(images) <- imgNames

grey_imgs<-lapply(img_transposed, greyscale, contrast = 0.2)
display(grey_imgs[[1]])

#f=array(1,dim=c(3,3))
#f=f/sum(f)
#filter_imgs<-filter2(grey_imgs[[1]],f)
#display(filter_imgs)

binary<-function(x, adj = 0.5) {
  binary_img<- x > adj
  return(binary_img)
}
img_neg<-function(x) {
  imgneg<-max(x)-x
  return(imgneg)
}
#lp_imgs<-lapply(grey_imgs, lp_filter, size=51,sigma=1)
neg_imgs<-lapply(grey_imgs, img_neg)
display(neg_imgs[[2]])
binary_img<-lapply(neg_imgs, binary, adj = 0.4)
display(binary_img[[2]])

imagesMapped <- lapply(binary_img, mapped, threshold = 0.2) #background intensity threshold adjustment

img_watershed<-single_cell_convert(imagesMapped[[1]],w=50,h=50,offset=0.001,areathresh=250,tolerance=0.8,ext = 3)
seed_watershed<-single_cell_convert(imagesMapped[[2]])
display(img_watershed)
display(seed_watershed)
final_img<-count_images(img_watershed,normalize = T, removeEdgeCells = T)
display(final_img)
display(st)

seed.input<-lapply(seed.input,as.numeric)
seed.input<-as.data.frame(seed.input)
seed.input<-round(seed.input,0)
seed.mtx<-circle_matrix(1280,1024,c(seed.input[1,1]),c(seed.input[1,2]),5,f=1)

for (j in 2:nrow(seed.input)){
  S1.mtx<-circle_matrix(1280,1024,c(seed.input[j,1]),c(seed.input[j,2]),5,f=1)
  point.select<-which(S1.mtx==1,arr.ind = TRUE)
  point.select<-as.data.frame(point.select)
  seed.mtx[ as.matrix(point.select) ] <- 1
}

seed.mtx<-seed.mtx[,c(1024:1),drop = FALSE]

display(seed.mtx)
display(imagesMapped[[2]])

seed.mtx.img<-Image(seed.mtx)
display(seed.mtx.img)
seed_img<-single_cell_convert(seed.mtx.img)
display(seed_img)

ctmask<-opening(img_watershed>0.1,makeBrush(5,shape='disc'))
cmask<-propagate(neg_imgs[[1]],seeds=seed_img,mask=ctmask,lambda = 10^1)
display(cmask)
segmented<-paintObjects(cmask,grey_imgs[[1]],col = c('pink','red'))
display(segmented,all=TRUE)
st <- stackObjects(cmask,img_watershed)
display(st)

#max(bwlabel(img_watershed))
#table(bwlabel(img_watershed))
#final_img<-count_images(img_watershed,normalize = T, removeEdgeCells = T)
#display(final_img)

#for (z in 1:length(images)) {
#  Index<-paste0(sub(".png", replacement = "", x=imgNames[[z]]))
#  Index1<-paste0(sub(".png", replacement = "/ ", x=imgNames[[z]]))
#  newpath<-file.path(image_savdir,Index1)
#  if(!dir.exists(newpath)){
#    dir.create(newpath)
#  }
#  image <- thresh(imagesMapped[[1]], w = 50, h = 50, offset = 0.001)
#  display(image)
#  image1 <- fillHull(image)
#  display(image1)
#  image2 <- watershed(distmap(image1), tolerance = 0.8, ext = 2) #numbers taken from Anabena
#  display(image2)
#  nf<-computeFeatures.shape(image2)
#  nr <- which(nf[, "s.area"] < 150)
#  image3 <- rmObjects(image2, nr)
#  display(image3)
#  features.data<-computeFeatures(image3, imagesMapped[[1]])
#  features<-as.data.frame(features.data)
#  features<-cbind(features,frame_num=NA)
#  st <- stackObjects(image3, imagesMapped[[1]])
#  for(k in 1:dim(st)[3]) {
#    st_img<-st[, , k]
#    analyzed_image1<-paste0(sub(".png", replacement = " ", x=imgNames[[z]]),"_frame")
#    analyzed_image2<-paste0(sub(".png", replacement = " ", x=analyzed_image1),k)
#    analyzed_image3<-paste0(sub(".png", replacement = " ", x=analyzed_image2),"_analyzed.tiff")
#    features$frame_num[k]<-cbind(k)
#    writeImage(st_img,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
#  }
#  csv_save<-paste0(paste(Index,Sys.Date()),".csv")
#  write.csv(features, paste0(savdir, csv_save)) #Change this CSV file name
#}
