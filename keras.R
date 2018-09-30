# Kian/Table 17.  Dog breed image recognition with keras/imagenet 
#
# 1. Read in image files and pre-process them
# 2. Use resnet50 with imagenet (tried other models, this one fitted less doormats to the dogs...)
# 3. imagenet has lots of americanised dog names, had to map across to dogstrust definitions. Match rate 
#     would have been much better if we had had time to do this properly
# 4. No time to train results, so just check against actual labels 


library(keras)
library(tidyverse)


# 1.Read in data ------------------------------------------------------------

# Function to read in data as array
image_preprocessor = function(image_path) {
  image = image_load(image_path, target_size = c(224,224)) %>%
    image_to_array()
  
  image <- array_reshape(image, c(1, dim(image))) %>%
    imagenet_preprocess_input()
  return(image)
}


# Locations of files to be read in
image_paths <- list.files('Dog Photos', recursive = TRUE, full.names = TRUE) 

# Get actual dog breed labels for dataset
image_labels<- lapply(image_paths, function(x)  {sub(' ','',sub('.*/', '', sub('\\(.*', '', x)))} )
# breeds <- sub(" .*", "", image_paths) # alternative from other team, shorter

# Reading in data, my RAM was not enough to do more than 250
data1 <- lapply(image_paths[1:250],image_preprocessor)

# 2. Model and Predictions ---------------------------------------------------

# choose pre-trained resnet50
model = application_resnet50(weights = 'imagenet')

# get model prediction
preds = lapply(data1, function(i) {
  imagenet_decode_predictions(predict(model, i), top = 1)[[1]]
})

# convert list of predictions to df and drop class name column
pred_df <- do.call(rbind, preds)
pred_df$class_name <- NULL


# 3. Clean up american names and spaces --------------------------------------
pred_df$actual <- tolower(image_labels[1:250])
pred_df$class_description <- tolower(str_replace(pred_df$class_description, "[[:punct:]]", " ")) 

pred_df$class_description <-  str_replace(pred_df$class_description, "[[:space:]]", "")
pred_df$actual <-  str_replace(pred_df$actual, "[[:space:]]", "")
pred_df1 <- pred_df

pred_df1 <- pred_df1 %>% 
  mutate(class_description = case_when(class_description == "bordercollie" ~ "collie", 
                                      class_description != "bordercollie" ~ class_description)) %>%
  
  mutate(class_description = case_when(class_description == "bullmastiff" ~ "doguedebordeaux", 
                                       class_description != "bullmastiff" ~ class_description)) %>%
  
  mutate(class_description = case_when(class_description == "doberman" ~ "dobermann", 
                                       class_description != "doberman" ~ class_description)) %>%
  
  mutate(class_description = case_when(class_description == "blenheimspaniel" ~ "cavalierkingcharles spaniel", 
                                       class_description != "blenheimspaniel" ~ class_description)) %>%
  
  mutate(class_description = case_when(class_description == "eskimodog" ~ "akita", 
                                       class_description != "eskimodog" ~ class_description)) %>%
  mutate(eval = case_when(actual == class_description ~ 1, 
            actual != class_description ~ 0))



# 4. Eval and group by breeds ------------------------------------------------

accuracy <- pred_df1 %>% filter(eval == 1) %>% nrow() / nrow(pred_df1)

accurary_breed <- pred_df1 %>%
  group_by(actual) %>%
  summarise(total = sum(eval),freq = n()) %>%
  mutate(accuracy = total/freq*100) 



