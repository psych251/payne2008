library(readxl)
library(tidyverse)
# link to the original stimuli list on Kensinger's labpage: https://www2.bc.edu/elizabeth-kensinger/NegNeutScenes.html

#read the processed list spreedsheet
final_list<- read_excel("list_all_files_in_use.xlsx")
#combine the conventional filename and the adhoc filenames
final_list<- final_list %>% 
  mutate(scene_final_raw = ifelse(is.na(adhoc_filenames), filename1_raw, adhoc_filenames))
#make the list of .png files
final_list <- final_list %>% 
  mutate(scene_final = str_c(substr(scene_final_raw, 1, str_length(scene_final_raw)-5), ".png", sep = ""),
         recog_background = str_c(substr(background_file, 1, str_length(background_file)-5), ".png", sep = ""),
         recog_object = str_c(substr(recog_object_raw, 1, str_length(recog_object_raw)-5), ".png", sep = ""))

# a list of new objects in .png
new_object = c()
for (name in final_list$new_object) {
  if(name != "na"){
    new_object = append(new_object, str_c(substr(name, 1, str_length(name)-5), ".png", sep = ""))
  }
}
# a list of new backgrounds
new_background = c()
for (name in final_list$new_background) {
  if(name != "na"){
    new_background = append(new_background, str_c(substr(name, 1, str_length(name)-5), ".png", sep = ""))
  }
}
# combine new objects and backgrounds
final_list$new_item = append(new_object,new_background[1:32])

link_pre = '<img src="'
link_post = '" style="width: 450px; height: 600px;">'
link_website = "http://web.stanford.edu/~jzhang18/psych251/my_tasks/pic/processed/"

# generate the html syntax for including each image file
final_list <- final_list %>% 
  mutate(scene_final_link = str_c(link_pre, link_website, scene_final, link_post),
         recog_object_link = str_c(link_pre, link_website, recog_object, link_post),
         recog_background_link = str_c(link_pre, link_website, recog_background, link_post),
         new_item_link = str_c(link_pre, link_website, new_item, link_post))

#output the final list
write_csv(final_list, "final_list.csv")

#create a list of all items in Session 2
#item: 64 recog_object, 64 recog_background, 64 new items
item <- append(final_list$recog_object, final_list$recog_background)
item <- append(item, final_list$new_item)

#create a list of all item links in Session 2
item_link <- append(final_list$recog_object_link, final_list$recog_background_link)
item_link <- append(item_link, final_list$new_item_link)

#import the questions from the excel spreedsheet
questions <- read_excel("Payne_Kensinger_stimuli_list.xlsx", 3, range = cell_cols("A:B"))
names(questions)[2] = "item"

#create a dataframe of items, item links, and their corresponding questions
questions_list <- data.frame("item_filname" = item,"item_link" = item_link)
#find the corresponding question
for (i in 1:nrow(questions_list)){
  for (j in 1:nrow(questions)){
    tmp_item = questions_list$item[i]
    tmp_name = questions$item[j]
    if (str_sub(tmp_item, 1, str_length(tmp_item)-5) == str_sub(tmp_name, 1, str_length(tmp_name)-6)){
      questions_list$question[i] = questions$question[j]
      #print(sprintf("%d %d", i, j)) #show the i-j pair
      break
    }
  }
}

# add labels to each item
# create item list
questions_list <- questions_list %>% 
  mutate(item_number = row_number())
# the counterbalancing list
ntr1bgd1 = c(1:8) # version 1 of neutral and version 1 of background
ntr2bgd1 = c(9:16) # version 2 of neutral and version 1 of background
neg1bgd1 = c(17:24)
neg2bgd1 = c(25:32)
ntr1bgd2 = c(33:40)
ntr2bgd2 = c(41:48)
neg1bgd2 = c(49:56)
neg2bgd2 = c(57:64) 
new_bgd = c(161:192)
# create component (object or background)
questions_list <- questions_list %>% mutate(component = ifelse(item_number %in% c(1:64, 129:160),
                                                     "object", "background"))
# create valence (neutral or negative)
questions_list <- questions_list %>% 
  mutate(valence = ifelse(item_number %in% 
                            c(ntr1bgd1, ntr2bgd1, ntr1bgd2, ntr2bgd2,  #object
                              ntr1bgd1+64, ntr2bgd1+64, ntr1bgd2+64, ntr2bgd2+64, #background
                              ntr1bgd1+128+16, ntr2bgd1+128+16, new_bgd), #new items
                          "neutral", "negative"))
# create item type (same, similar, or new)
questions_list <- questions_list %>% 
  mutate(type = ifelse(item_number %in% c(ntr1bgd1, neg1bgd1, ntr1bgd2, neg1bgd2, 
                                          ntr1bgd1+64, ntr2bgd1+64, neg1bgd1+64, neg2bgd1+64),"same", 
                       ifelse(item_number %in% 129:192, "new", "similar") ))


# output the question list
write_csv(questions_list, "questions_list.csv")


# practice files
practice <- data.frame("scene" = c("zoo.png","show.png"), "item" = c("polar_bear.png","rhinoceros.png", "desert.png", "tennis court.png"))
practice <- practice %>% 
  mutate(scene_link = str_c(link_pre, link_website, scene, link_post),
         item_link = str_c(link_pre, link_website, item, link_post),
         question = str_c("Did you see a ", str_sub(item, 1, str_length(item)-4),"?"))
#export practice list
write_csv(practice, "practice_list.csv")
