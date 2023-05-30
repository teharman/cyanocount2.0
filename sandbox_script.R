library(EBImage)
library(tiff)
library(png)
library(pixmap)
library(raster)
library(cellcount)
library(dplyr)
library(jjb)
library(shiny)
library(magick)
library(ggplot2)
library(shinyalert)
library(shinyFiles)
library(shinyjs)
library(DT)

# clear environment and free unused memory
gc()

####Change directories/Import images####
img_dir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/version_2/5_4_demo/NZ_Anabena/40x_Nikon_TE300/")
image_savdir <- ("C:/Users/Tyler.Harman/Desktop/cellcount_work/version_2/5_4_demo/NZ_Anabena/40x_Nikon_TE300")
images <- list.files(img_dir, pattern = "tif", full.name = T)
images_names <- list.files(img_dir, pattern = "tif", full.name = F)

imgNames <- paste0(images_names)
read_images <- lapply(images, readTIFF)
img_transposed <- lapply(read_images,aperm,c(2,1,3))
names(images) <- imgNames

#img number
y<-5

####Initial image conversion####
grey_imgs<-lapply(img_transposed, greyscale, contrast = 0.2)
display(grey_imgs[[y]])

binary<-function(x, adj = 0.5) {
  binary_img<- x > adj
  return(binary_img)
}
img_neg<-function(x) {
  imgneg<-max(x)-x
  return(imgneg)
}
neg_imgs<-lapply(grey_imgs, img_neg)
display(neg_imgs[[y]])
binary_img<-lapply(neg_imgs, binary, adj = 0.75)
display(binary_img[[y]])

imagesMapped <- lapply(binary_img, mapped, threshold = 0.2) #background intensity threshold adjustment

img_watershed<-single_cell_convert(imagesMapped[[y]],w=50,h=50,offset=0.001,areathresh=250,tolerance=0.8,ext = 3)
display(img_watershed)

####Shiny UI cell selector####

jscode <- "shinyjs.closeWindow = function() { window.close(); }"
seed.input <<- data.frame(x_axis = numeric(0), y_axis = numeric(0))

create_image <- function(loaded_image, image_data) {

  displayed_image <- loaded_image +
    geom_point(data = image_data, aes(x = .data$x_values,
                                      y = .data$y_values)) +
    geom_path(data = image_data, aes(x = .data$x_values,
                                     y = .data$y_values
    ),
    color = "black") +
    geom_point(data=seed.input,aes(x=.data$x_axis,
                                   y=.data$y_axis),
               color="red")
  return(displayed_image)
}

ui1 <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  sidebarLayout(
    sidebarPanel(
      actionButton("close", "Close window",class = "btn-danger",style='height:75px;width:175px;font-size:140%',icon=icon("check"),style="display:center-align"),
      actionButton("BRefresh","Refresh",class = "btn-success",style='height:75px;width:175px;font-size:140%',icon=icon("arrows-rotate"),style="display:center-align"),
      h3(" "),
      DT::dataTableOutput('data')
    ),
    mainPanel(
      plotOutput("current_image_plot", dblclick = "double_click", hover = "hover", width = "100%"),
      h3(" "),
      h3(" ")
    )
  )
)

server1 <- function(input, output, session) {

  image_data <- shiny::reactiveValues()
  image_data$double_click <- data.frame(x_values=c(NA_real_,NA_real_), y_values = c(NA_real_,NA_real_))

  loaded_image <- magick::image_ggplot(image_modulate(image_read(images[[y]]),brightness=1000))

  output$current_image_plot <- renderPlot({
    displayed_image <- create_image(loaded_image,
                                    image_data$double_click
    )
    return(displayed_image)
  }, height=800,width=1000)

  rv <- reactiveVal(seed.input)

  output$data <- DT::renderDataTable ({
    DT::datatable(rv(), editable = TRUE)
  })

  observeEvent({input$double_click}, {
    clickrow <<- data.frame(x_values = input$double_click$x,
                            y_values = input$double_click$y)

    seed.input[nrow(seed.input) + 1, ] <<- c(clickrow$x_values,clickrow$y_values)

    image_data$double_click[1,] <<- clickrow

  })

  onclick("BRefresh",{
    proxy=dataTableProxy("data")
    replaceData(proxy,seed.input)
  })

  observeEvent(input$close, {
    js$closeWindow()
    shinyjs::stopApp()
  })

  session$onSessionEnded(function() {
    stopApp()
  })

}

runGadget(ui1, server1, viewer = dialogViewer("cellcount Image Analysis Interface",
                                              width = 1500, height = 2000))

####Seed segmentation####
seed.input<-lapply(seed.input,as.numeric)
seed.input<-as.data.frame(seed.input)
seed.input<-round(seed.input,0)
seed.mtx<-circle_matrix(2688,2200,c(seed.input[1,1]),c(seed.input[1,2]),5,f=1)

for (j in 2:nrow(seed.input)){
  S1.mtx<-circle_matrix(2688,2200,c(seed.input[j,1]),c(seed.input[j,2]),5,f=1)
  point.select<-which(S1.mtx==1,arr.ind = TRUE)
  point.select<-as.data.frame(point.select)
  seed.mtx[ as.matrix(point.select) ] <- 1
}

seed.mtx<-seed.mtx[,c(2200:1),drop = FALSE]

display(seed.mtx)

seed.mtx.img<-Image(seed.mtx)
display(seed.mtx.img)
seed_img<-single_cell_convert(seed.mtx.img)
display(seed_img)

ctmask<-opening(img_watershed>0.1,makeBrush(5,shape='disc'))
cmask<-propagate(neg_imgs[[y]],seeds=seed_img,mask=ctmask,lambda = 10^1)
display(cmask)
segmented<-paintObjects(cmask,grey_imgs[[y]],col = c('pink','red'))
display(segmented,all=TRUE)
st_blob <- stackObjects(cmask,img_watershed)
st_img <- stackObjects(cmask,grey_imgs[[y]])
st_img_test<-Image(st_img)
display(st_blob)
display(st_img)

####Shiny UI to remove certain cells####

jscode <- "shinyjs.closeWindow = function() { window.close(); }"
image_number<<-data.frame(img_num=numeric(0))
image_num1<<-textConnection('image_num2','wr',local=FALSE)

ui2 <- fluidPage(
  useShinyjs(),
  extendShinyjs(text = jscode, functions = c("closeWindow")),
  sidebarLayout(
    sidebarPanel(
      actionButton("previous", "Previous", icon=icon("arrow-left")),
      actionButton("next", "Next", icon=icon("arrow-right")),
      h6(" "),
      h6(" "),
      textOutput("image_num"),
      h6(" "),
      h6(" "),
      actionButton("run1", "Select Image Removal",class = "btn-success",icon=icon("trash")),
      h6(" "),
      h6(" "),
      actionButton("close", "Close window",class = "btn-danger",icon=icon("check"))
    ),
    mainPanel(
      plotOutput("current_image_plot")
    )
  )
)

server2 <- function(input, output, session) {
  index <- reactiveVal(1)

  observeEvent(input[["previous"]], {
    index(max(index()-1, 1))
  })

  observeEvent(input[["next"]], {
    index(min(index()+1, dim(st_img_test)[3]))
  })

  output$current_image_plot <- renderPlot({
    loaded_image <- magick::image_ggplot(image_read(st_img_test[,,index()]))
    loaded_image
  },res=300,width=350,height=350)

  output$image_num<-renderText({
    paste('Image number: ',index())
  })

  observeEvent(input$run1, {
    shinyalert::shinyalert('Enter image numbers for removal',
                           type='input',callbackR=image_number, showCancelButton = TRUE,
                           inputPlaceholder = 'Example: 1, 2, 3, 4, 5',
                           size="m")
  })

  observeEvent(input[["run2"]], {
    image_number[nrow(image_number) + 1, ] <<- c(index())
  })

  image_number<-function(value4) {
    sink(image_num1,0)
    save_name<-cat(value4)
    sink()
    close(image_num1)
    image_num2<<-scan(text=image_num2,dec=",")
  }

  observeEvent(input$close, {
    js$closeWindow()
    shinyjs::stopApp()
  })

}

runGadget(ui2, server2, viewer = dialogViewer("Cell Image Selector",
                                              width = 800, height = 400))

#Removal of problematic images from output of 'image_select' Shiny UI
st_blob_rm<-st_blob[,,-image_num2]
st_img_rm<-st_img[,,-image_num2]

####Create features and export images####
features.data.blob<-computeFeatures(cmask,img_watershed)
features.data.img<-computeFeatures(cmask,grey_imgs[[y]])
features.blob<-as.data.frame(features.data.blob)
features.img<-as.data.frame(features.data.img)
features.blob1<-cbind(features.blob,frame_num=NA)
features.img1<-cbind(features.blob,frame_num=NA)

#removal of rows from the shiny_select UI
features.blob2<-features.blob1 %>%  filter(!row_number() %in% image_num2)
features.img2<-features.img1 %>% filter(!row_number() %in% image_num2)

#blob st img save
Index_blob<-paste0(sub(".tif", replacement = "blob_", x=imgNames[[y]]))
Index1<-paste0(sub(".tif", replacement = "/ ", x=imgNames[[y]]))
newpath<-file.path(image_savdir,Index1)
if(!dir.exists(newpath)){
  dir.create(newpath)
}
for(k in 1:dim(st_blob_rm)[3]) {
  st_imgs_blob<-st_blob_rm[, , k]
  analyzed_image1<-paste0(sub(".tif", replacement = " ", x=imgNames[[y]]),"_frame")
  analyzed_image2<-paste0(sub(".tif", replacement = " ", x=analyzed_image1),k)
  analyzed_image3<-paste0(sub(".tif", replacement = " ", x=analyzed_image2),"_blob_analyzed.tiff")
  features.blob2$frame_num[k]<-cbind(k)
  writeImage(st_imgs_blob,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
}
csv_save<-paste0(paste(Index_blob,Sys.Date()),".csv")
write.csv(features.blob2, paste0(newpath, csv_save)) #Change this CSV file name

#grey st img save
Index_grey<-paste0(sub(".tif", replacement = "grey_", x=imgNames[[y]]))
Index1<-paste0(sub(".tif", replacement = "/ ", x=imgNames[[y]]))
newpath<-file.path(image_savdir,Index1)
if(!dir.exists(newpath)){
  dir.create(newpath)
}
for(k in 1:dim(st_img_rm)[3]) {
  st_imgs_grey<-st_img_rm[, , k]
  analyzed_image1<-paste0(sub(".tif", replacement = " ", x=imgNames[[y]]),"_frame")
  analyzed_image2<-paste0(sub(".tif", replacement = " ", x=analyzed_image1),k)
  analyzed_image3<-paste0(sub(".tif", replacement = " ", x=analyzed_image2),"_grey_analyzed.tiff")
  features.img2$frame_num[k]<-cbind(k)
  writeImage(st_imgs_grey,files = paste0(newpath, analyzed_image3),compression=c("LZW"))
}
csv_save<-paste0(paste(Index_grey,Sys.Date()),".csv")
write.csv(features.img2, paste0(newpath, csv_save)) #Change this CSV file name

#
#
#
#
#
#
#
#
#

####Misc code####

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
