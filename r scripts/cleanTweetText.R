cleanTweetText<-function(text){
  require(stringr)
  require(stringi)
  library(tm)
  library(SnowballC)
    hashtag_pattern <- "#([[:alnum:]]|[_])+"
    mention_pattern <- "@([[:alnum:]]|[_])+"
    strip_RT_pattern<-"RT\\s@([[:alnum:]]|[_])+:"

    text<-gsub("<e4>","ä",text)
    text<-gsub("<c4>","Ä",text)
    text<-gsub("<d6>","Ö",text)
    text<-gsub("<dc>","Ü",text)
    text<-gsub("<f6>","ö",text)
    text<-gsub("<fc>","ü",text)
    text<-gsub("<df>","ss",text)
    text<-gsub("ß","ss",text)
    text<-gsub("ã","ä",text)
    
    text<-gsub("<[^\\s]+>","",text)
    text<-gsub("<[^\\s]+","",text)
    
    text<-gsub("&amp;","",text)
    
    text<-gsub("http[s]?\\://t\\.co/[^ ]{10}","",text)
    text<-gsub("http.*[^\\s]+","",text)
    text<-gsub("htt[p]?\U2026","",text)
    text<-gsub("https","",text)
    text<-gsub("http","",text)
    
    text<-ifelse(grepl("\U2026",text),"",text)
      
    
  #Die URLs und Mentions werden entfernt
  text<-str_replace_all(text,pattern=mention_pattern,replacement="")
  #Wörter die weniger als 3 Zeichen haben müssen weg außer sie haben #
  text<-str_replace_all(text,pattern="(?<!#)\\b[a-zA-Z0-9]{1,2}\\b",replacement = "")
  #depends on whether you wanna keep hashtags or not
  text<-str_replace_all(text,pattern="#",replacement="")
  #Konvertierung von Umlauten
  text<-stri_replace_all_fixed(text, 
                                  #c("?", "?", "?", "?", "?", "?"),
                                 c("\U00E4","\U00F6","\U00FC","\U00C4","\U00D6","\U00DC"),
                                  c("ae", "oe", "ue", "Ae", "Oe", "Ue"), vectorize_all = FALSE)

  #Zahlen außer IN Hashtags
  text<-str_replace_all(text,pattern="\\b\\d+\\b",replacement="")
  #Satzzeichen und Special Characters außer # müssen weg
  text<-str_replace_all(text,pattern="[^[:alnum:]#]",replacement=" ")
  text<-str_to_lower(text)
  return(text)
}

stemTweetText<-function(text){
  require(stringr)
  require(stringi)
  library(tm)
  library(SnowballC)
  text<-as.character(text)
  text<-strsplit(text," ")
  words<-unlist(text)
  words<-words[words!=""]
  words<-wordStem(words,language="german")
  words<-paste(words,collapse=" ")
  return(words)
}

cleanCorpus<-function(text){
  require(stringr)
  require(stringi)
  text<-gsub("<e4>","ä",text)
  text<-gsub("<c4>","Ä",text)
  text<-gsub("<d6>","Ö",text)
  text<-gsub("<dc>","Ü",text)
  text<-gsub("<f6>","ö",text)
  text<-gsub("<fc>","ü",text)
  text<-gsub("<df>","ss",text)
  text<-gsub("ß","ss",text)
  
  text<-gsub("<[^\\s]+>","",text)
  text<-gsub("<[^\\s]+","",text)
  
  text<-gsub("&amp;","",text)
  
  text<-stri_replace_all_fixed(text, 
                               #c("?", "?", "?", "?", "?", "?"),
                               c("\U00E4","\U00F6","\U00FC","\U00C4","\U00D6","\U00DC"),
                               c("ae", "oe", "ue", "Ae", "Oe", "Ue"), vectorize_all = FALSE)
  text<-str_replace_all(text,pattern="[^[:alnum:]\\#|\\@]",replacement=" ")
  return(text)
}
