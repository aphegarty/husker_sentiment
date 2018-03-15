#this script outputs a "clean" sampleMax forum tables with...
  #1. only football posts, if including text included in footballTerms list
  #2. cleaned posts, no quotes, links, or formatting
  #3. unix time converted to readable dates
  #4. sentiment calculated in syuzhet, afinn
  #5. add field with three coach names, true if they are mentioned

#need massive fix on football term matching. likely can be fixed by putting a space before and after each word by modifying "|" to " | " and replacing ^ and $ with a space as well as punctuation

#imports
library(data.table) #large data file
library(anytime) # convert from unix
library(dplyr) 
library(syuzhet) #sentiment analysis
library(tidytext) #for stop_words
#info on data.tables, a better version of R's dataframe at
#https://github.com/Rdatatable/data.table/wiki

#load file fread is much faster than read, but is a data.table not dataframe
sampleMax <- fread("data/sampleMax.csv", header = FALSE)

#uh, so let's delete most the data we just imported. Appears to be metadata
sampleMax[,V2:=NULL]
sampleMax[,V3:=NULL]
sampleMax[,V7:=NULL]
sampleMax[,V8:=NULL]
sampleMax[,V9:=NULL]
sampleMax[,V10:=NULL]
sampleMax[,V11:=NULL]
sampleMax[,V12:=NULL]
sampleMax[,V13:=NULL]
sampleMax[,V14:=NULL]
sampleMax[,V15:=NULL]
sampleMax[,V16:=NULL]
sampleMax[,V17:=NULL]

#name remaining fields
names(sampleMax)[1]<-"key"
names(sampleMax)[2]<-"user"
names(sampleMax)[3]<-"unixTime"
names(sampleMax)[4]<-"postText"

#time it, create field called time
sampleMax$unixTime <- as.integer(sampleMax$unixTime)
sampleMax <- mutate(sampleMax, time = anytime(unixTime))

#remove junk from postText
sampleMax$postText <- gsub("(?s)\\[img\\].*?\\[\\/img\\]"," ", sampleMax$postText, ignore.case = TRUE, perl=T)
sampleMax$postText <- gsub("(?s)\\[url\\].*?\\[\\/url\\]"," ", sampleMax$postText, ignore.case = TRUE, perl=T)
sampleMax$postText <- gsub("(?s)\\[quote.*?\\].*?\\[\\/QUOTE\\]"," ", sampleMax$postText, ignore.case = TRUE, perl=T)
sampleMax$postText <- gsub("\\[.*?\\]"," ", sampleMax$postText, ignore.case = TRUE, perl=T)
sampleMax$postText <- gsub("[\\[<].*?[>\\]]"," ", sampleMax$postText, ignore.case = TRUE, perl=T)
sampleMax$postText <- gsub("(http|www).*"," " , sampleMax$postText, ignore.case = TRUE)

#create lowercase, no punctuation field to find football terms
sampleMax <- mutate(sampleMax, words = tolower(postText))
sampleMax$words <- gsub("\\.|!|\\?|,|;|\\*", " ", sampleMax$words, ignore.case = TRUE)
sampleMax$words <- gsub("'|-", "", sampleMax$words, ignore.case = TRUE)

#eliminate everything not about football
footballTerms = c("football",
      "pelini","bo", "riley","scott", "frost", "callahan", "solich", "osborne", #coaches
      "eichorst", "moos", #ADs 
      "walters", "langsdorf", "watson", "beck", "offensive coordinator",#offensive coordinators
      "diaco", "banker", "papuchis", "carl", "chinander", "defensive coordinator",#defensive coordinators
      "first down", "1st down", "second down", "2nd down", "third down", "3rd down", "fourth down", "4th down", "downs",
      "safet", "field goal", "touchdown", "touch down", "td", "fg", "extra point", "conversion",
      "memorial stadium", "midfield", "mid field", "endzone", "end zone", "red zone", "redzone", "yard", "touchback", "touch back", "special team", "punt", "kick",
      "balloons", "tunnelwalk", "tunnel walk", "sea of red",
      "heisman", "bowl", 
      "chop block", "roughing", "targeting", "flag", "holding", "offside", "pass interference", "pi ", "penalty", "penalties",
      "illegal block", "block in the back", "clipping", "delay of game", "false start", "encroachment", "face mask", "facemask", #more penalties
      "horse collar", "horse-collar", "illegal formation", "hands to the face", "intentional grounding", "neutral zone", "unsportsmanlike conduct",
      "pass", "run", #yes these in general will be used for a lot of sports but i think mostly in football, take out if ill-adviser
      "helmet", "shoulder pad", #uniforms
      "back field", "backfield", "dead ball", "live ball",
      "ypc", "yac", 
      "fbs ", "fcs ", "lateral", "line of scrimmage", "linemen", "lineman",
      "play-action", "play action", "rush ", "scramble", "scrambled", "scrambling", "snap",
      "frazier", "rozier", "crouch", "johnny rodgers", #historic
      "wide open", "coverage",
      "and short", "and long", "and goal", "& short", "& long", "& goal",
      "roll out",
      "audible", "blocking", #not simply block because of other sports 
      "blitz", "chains", "complete", #includes incomplete #other terms
      "o-line", "oline", #no guard or center because of basketball
      "wr", "reciever", "receiver", "tight end", "tightend", "te",
      "running back", "ball carrier", "rb", "hb", "iback", "i-back", "halfback", "half back", "runningback",
      "d-line", "dline", "defensive end", "de", "dt", "dl",
      "nickle", "defensive back", "db", "s", "fs",   
      "linebacker", "line backer", "lb", "olb", 
      "passer", "quarter back", "quarterback", "qb",
      "sack", "reception", "rush", "tackle", "play calling", 
      "interception", "intercepted", "pick six", "pick 6", "pick off", "picked off", "fumble", #s #d
      "lee", "green", #2009qb 
      "rex", "burkhead", "helu", #2009ru
      "paul", "mcneill", "gilleylen", #2009wr
      "suh", "dillard", "asante", "crick", "ohanlon", #2009tckl
      "gomes", "amukamara", "hagg", "dillard", #2009int
      "tm", "martinez", "t-magic", "tmagic", "t-tragic", "ttragic", #Taylor Martinez
      "kinnie", "reed", #2010 rb, wr
      "meredith", "steinkuhler", "cassidy", "thenarse", #2010d
      "abdullah", "bell", "enunwa", "turner", "cotton", #2011o "Heard" too common
      "compton", "stafford", "jean-baptiste", "thorell", #2011d
      "henry",
      "rk3", "rkiii", "kellogg", #qb ron kellogg
      "imani", "janovich", #cross too common
      "whaley", "martin", "evans", "ankrah", "mitchell",
      "maher", #2012
      "armstrong", "tommy", "ta", #2013 qb armstrong added
      "newby", "westerkamp", "burtch", "carter",
      "hail mary", "westercatch", #2013
      "cooper", "santos", "gregory", "rose ", "gerry", "moss ", "mitchell",
      "ryker", "fyfe", "stanton", #qbs
      "foltz", #RIP
      "pierson-el", "dpe", "demornay", "reilly", "alonzo", #moore too common
      "roach", "banderas", "mcmullen", "collins", "valentine", "davie", "cockrell",
      "brown", "bondi",
      "alex lewis",
      "ozigbo", "wilbon", "devine", "morgan", "hovey",
      "kalu", "weber", "maliek", "dedrick", #dedrick young too common
      "jones", 
      "bryant", "rahn", "reimers", #2016
      "maurice", "dzuris", "freedom", "akinmoladum", "lamar", "mcnitt",
      "lightbourn", "lightborn",
      "pob", "obrien", "tl",#2017
      "stille", "gifford",
      "aaron williams", "keyan williams",
      "dismuke", "alex davis", "khalil", "khalil davis", "carlos davis", "bootle",
      "spielman", "hoppes", "lindsey", "tyjon", "bradley",
      #Top recruits
      "recruit", #yes, for all sports, buts lets be real people only follow football
      "jurgens", "maurice washinton", "camron jones", "wildeman", "caleb tannor", "honas", "farniok", #martinez already in
      "gebbia", "mcquitty", "keyshawn", "jaimes", "sichterman", "bando", "ben miles", "elijah blades",
      "raridon", "dismuke", "brokop", "domann", "engelhaupt", "tony butler", "quayshon alexander", "jack stoll", "derrion grim",
      #2014
      "tanner farmer", "nick gates", "jerald foster", "peyton newell", "keels", "darlington", 
      "stoltenberg", "tolbert", "sedrick king", "aj bush", "monte harrison", 
      #2015
      "jalin barnett", "jordan stevenson", "matt snyder", "alex davis", "tyrin ferguson",
      #2013
      "adam taylor", "natter", "singleton", "knevel", 
      "courtney love", "greg hart", "boaz joseph", "kondolo", 
      #2012
      "thurston", "rose-ivey", "afalva", "seisay", 
      "thomas brown", "aaron curry", "whitaker",
      #2011
      "bubba", "starling", "tyler moore", "todd peat", "reeves", "klachko", "sterup",
      "david sutton", "bondi", "taariq", "givens price", "max pirman", "chase harper", 
      #2010
      "braylon", "andrew rodriguez", "jermarcus hardrick", "chase rome", "carnes", "ciante evans",
      "ankrah", "sirles", "dejon gomes", "spencer long", "josh williams", "marlowe")

#select posts with football terms
yuckyregex <- paste(footballTerms, collapse = "\\b|\\b") #close enough
footballMax <- sampleMax %>% filter(grepl(yuckyregex, words))

#filter out stop words
grossregex1 <- paste(head(stop_words$word,n=575), collapse = "\\b|\\b") #there are (575*2) - 1 stop words
grossregex2 <- paste(tail(stop_words$word,n=575), collapse = "\\b|\\b")
footballMax$words <- gsub(grossregex1, " ", footballMax$words, ignore.case = TRUE)
footballMax$words <- gsub(grossregex2, " ", footballMax$words, ignore.case = TRUE)

#notice we are now working with the table footballMax, original table not deleted

footballMax <- mutate(footballMax,
        syuzhet = get_sentiment(footballMax$words, method = "syuzhet"),
        afinn = get_sentiment(words, method="affin"), 
        #get_sentiment afinn simply adds the word's values, we need to divide by the count so longer words do not have more positive sentiment
        #afinn = (get_sentiment(words,method="afinn")) / (length(get_tokens(words))),
        pelini = grepl("\\bpelini\\b|\\bbo\\b", footballMax$words),
        riley = grepl("\\briley\\b", footballMax$words),
        frost = grepl("\\bfrost\\b", footballMax$words))

#outputfile
fwrite(footballMax, file = "data/real_football_only.csv")
