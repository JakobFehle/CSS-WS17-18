cleanHashtags<-function(hashtag){
  require(stringr)
  require(stringi)
  
  hashtag<-str_replace_all(hashtag,pattern="https",replacement = "")
  hashtag<-str_replace_all(hashtag,pattern="#",replacement = "")
  hashtag<-stri_replace_all_fixed(hashtag,
                                  c("\U00E4","\U00F6","\U00FC","\U00C4","\U00D6","\U00DC"),
                                  c("ae", "oe", "ue", "Ae", "Oe", "Ue"),
                                  vectorize_all = FALSE)
  hashtag<-stri_trans_general(hashtag, "latin-ascii")
  hashtag<-str_to_lower(hashtag)
  return(hashtag)
}