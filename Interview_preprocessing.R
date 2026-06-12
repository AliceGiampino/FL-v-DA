

############################# INTERVIEW ###############################

# inter: file containing the interview
# the original file contains the interview in Italian

table(inter$File.ID)

table(inter$File.ID, inter$Speaker)
# 1 to 1 correspondence

# Consider only File.ID:
inter <- inter[,-2]

# Initialize storage for processed text
inter$processed_final <- character(nrow(inter))

# Process each row of inter[,2] with all transformations
cat("\n=== Processing inter[,2] with all transformations ===\n")

for(i in 1:nrow(inter)) {

  # Get text from second column
  text <- inter[i, 2]

  # Skip if empty
  if(is.na(text) || is.null(text) || nchar(trimws(text)) == 0) {
    inter$processed_final[i] <- ""
    next
  }

  # STEP 1: Basic cleaning with tm
  text <- tolower(text)
  text <- gsub("ehm", "", text)
  text <- gsub("[[:punct:]]", "", text)
  text <- gsub("\\d+", "", text)
  text <- gsub("à", "a", text)
  text <- gsub("è", "e", text)
  text <- gsub("é", "e", text)
  text <- gsub("ì", "i", text)
  text <- gsub("ò", "o", text)
  text <- gsub("ù", "u", text)


  # STEP 2: Convert nouns ending in -ita to adjectives (facilita -> facile)
  text <- gsub("\\bfacilita\\b", "facile", text, ignore.case = TRUE)
  text <- gsub("\\bdifficolta\\b", "difficile", text, ignore.case = TRUE)
  text <- gsub("\\bnecessita\\b", "necessario", text, ignore.case = TRUE)
  text <- gsub("\\bgenerosita\\b", "generoso", text, ignore.case = TRUE)
  text <- gsub("\\bvelocita\\b", "veloce", text, ignore.case = TRUE)
  text <- gsub("\\bsemplicita\\b", "semplice", text, ignore.case = TRUE)
  text <- gsub("\\bcomplessita\\b", "complesso", text, ignore.case = TRUE)
  text <- gsub("\\bpossibilita\\b", "possibile", text, ignore.case = TRUE)
  text <- gsub("\\bprobabilita\\b", "probabile", text, ignore.case = TRUE)
  text <- gsub("\\bdisponibilita\\b", "disponibile", text, ignore.case = TRUE)
  text <- gsub("\\bresponsabilita\\b", "responsabile", text, ignore.case = TRUE)
  text <- gsub("\\bcapacita\\b", "capace", text, ignore.case = TRUE)
  text <- gsub("\\bcuriosita\\b", "curioso", text, ignore.case = TRUE)
  text <- gsub("\\bgravita\\b", "grave", text, ignore.case = TRUE)
  text <- gsub("\\bnovita\\b", "nuovo", text, ignore.case = TRUE)
  text <- gsub("\\bverita\\b", "vero", text, ignore.case = TRUE)
  text <- gsub("\\bnormalita\\b", "normale", text, ignore.case = TRUE)
  text <- gsub("\\bpersonalita\\b", "personale", text, ignore.case = TRUE)
  text <- gsub("\\battivita\\b", "attivo", text, ignore.case = TRUE)
  text <- gsub("\\bpassivita\\b", "passivo", text, ignore.case = TRUE)
  text <- gsub("\\bsensibilita\\b", "sensibile", text, ignore.case = TRUE)
  text <- gsub("\\bvisibilita\\b", "visibile", text, ignore.case = TRUE)
  text <- gsub("\\baccessibilita\\b", "accessibile", text, ignore.case = TRUE)
  text <- gsub("\\bcreativita\\b", "creativo", text, ignore.case = TRUE)
  text <- gsub("\\bproduttivita\\b", "produttivo", text, ignore.case = TRUE)

  # STEP 3: Convert adjectives to nouns ending in -ita (generoso -> generosita, necessario -> necessità)
  # Note: facile stays facile (not converted)
  text <- gsub("\\bgenerosa\\b", "generoso", text, ignore.case = TRUE)
  text <- gsub("\\bgenerosi\\b", "generoso", text, ignore.case = TRUE)
  text <- gsub("\\bgenerose\\b", "generoso", text, ignore.case = TRUE)

  text <- gsub("\\bnecessaria\\b", "necessario", text, ignore.case = TRUE)
  text <- gsub("\\bnecessari\\b", "necessario", text, ignore.case = TRUE)
  text <- gsub("\\bnecessarie\\b", "necessario", text, ignore.case = TRUE)

  # difficile -> difficoltà (but facile stays facile)
  text <- gsub("\\bdifficili\\b", "difficile", text, ignore.case = TRUE)

  text <- gsub("\\bveloci\\b", "veloce", text, ignore.case = TRUE)

  text <- gsub("\\bsemplici\\b", "semplice", text, ignore.case = TRUE)

  text <- gsub("\\bcomplessa\\b", "complesso", text, ignore.case = TRUE)
  text <- gsub("\\bcomplessi\\b", "complesso", text, ignore.case = TRUE)
  text <- gsub("\\bcomplesse\\b", "complesso", text, ignore.case = TRUE)

  text <- gsub("\\bpossibili\\b", "possibile", text, ignore.case = TRUE)

  text <- gsub("\\bprobabili\\b", "probabile", text, ignore.case = TRUE)

  text <- gsub("\\bdisponibili\\b", "disponibile", text, ignore.case = TRUE)

  text <- gsub("\\bresponsabili\\b", "responsabile", text, ignore.case = TRUE)

  text <- gsub("\\bcapaci\\b", "capace", text, ignore.case = TRUE)

  text <- gsub("\\bcuriosa\\b", "curioso", text, ignore.case = TRUE)
  text <- gsub("\\bcuriosi\\b", "curioso", text, ignore.case = TRUE)
  text <- gsub("\\bcuriose\\b", "curioso", text, ignore.case = TRUE)

  text <- gsub("\\bgravi\\b", "grave", text, ignore.case = TRUE)

  text <- gsub("\\bnuova\\b", "nuovo", text, ignore.case = TRUE)
  text <- gsub("\\bnuovi\\b", "nuovo", text, ignore.case = TRUE)
  text <- gsub("\\bnuove\\b", "nuovo", text, ignore.case = TRUE)

  text <- gsub("\\bvera\\b", "vero", text, ignore.case = TRUE)
  text <- gsub("\\bveri\\b", "vero", text, ignore.case = TRUE)
  text <- gsub("\\bvere\\b", "vero", text, ignore.case = TRUE)

  text <- gsub("\\bnormali\\b", "normale", text, ignore.case = TRUE)

  text <- gsub("\\bpersonali\\b", "personale", text, ignore.case = TRUE)

  text <- gsub("\\battiva\\b", "attivo", text, ignore.case = TRUE)
  text <- gsub("\\battivi\\b", "attivo", text, ignore.case = TRUE)
  text <- gsub("\\battive\\b", "attivo", text, ignore.case = TRUE)

  text <- gsub("\\bpassiva\\b", "passivo", text, ignore.case = TRUE)
  text <- gsub("\\bpassivi\\b", "passivo", text, ignore.case = TRUE)
  text <- gsub("\\bpassive\\b", "passivo", text, ignore.case = TRUE)

  text <- gsub("\\bsensibili\\b", "sensibile", text, ignore.case = TRUE)

  text <- gsub("\\bvisibili\\b", "visibile", text, ignore.case = TRUE)

  text <- gsub("\\baccessibili\\b", "accessibile", text, ignore.case = TRUE)

  text <- gsub("\\bcreativa\\b", "creativo", text, ignore.case = TRUE)
  text <- gsub("\\bcreativi\\b", "creativo", text, ignore.case = TRUE)
  text <- gsub("\\bcreative\\b", "creativo", text, ignore.case = TRUE)

  text <- gsub("\\bproduttiva\\b", "produttivo", text, ignore.case = TRUE)
  text <- gsub("\\bproduttivi\\b", "produttivo", text, ignore.case = TRUE)
  text <- gsub("\\bproduttive\\b", "produttivo", text, ignore.case = TRUE)

  # STEP 4: Convert conjugated verbs to infinitive
  # First handle compound verb forms (passato prossimo, etc.) - these must come BEFORE individual word conversions
  # Compound forms with "essere" (passato prossimo)
  text <- gsub("\\bsono stato\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsono stata\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsono stati\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsono state\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsei stato\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsei stata\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\be stato\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\be stata\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsiamo stati\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsiamo state\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsiete stati\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsiete state\\b", "essere", text, ignore.case = TRUE)

  # Compound forms with "avere" (passato prossimo)
  text <- gsub("\\bho avuto\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bhai avuto\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bha avuto\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\babbiamo avuto\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bavete avuto\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bhanno avuto\\b", "avere", text, ignore.case = TRUE)

  # Irregular verbs (individual word conversions)
  text <- gsub("\\bsono\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsei\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\be\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bé\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsiamo\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bsiete\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bero\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\beri\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bera\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\beravamo\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\beravate\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\berano\\b", "essere", text, ignore.case = TRUE)

  text <- gsub("\\bho\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bhai\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bha\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\babbiamo\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bavete\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bavevi\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bavevo\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\baveva\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bavevamo\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bavevate\\b", "avere", text, ignore.case = TRUE)

  text <- gsub("\\bvado\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bvai\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bva\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bvada\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bandiamo\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bandate\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bvanno\\b", "andare", text, ignore.case = TRUE)

  text <- gsub("\\bfaccio\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfai\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfa\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfacciamo\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfate\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfanno\\b", "fare", text, ignore.case = TRUE)

  text <- gsub("\\bdico\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdici\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdice\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdiciamo\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdite\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdicono\\b", "dire", text, ignore.case = TRUE)

  text <- gsub("\\bvengo\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvieni\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bviene\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bveniamo\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvenite\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvengono\\b", "venire", text, ignore.case = TRUE)

  text <- gsub("\\bsto\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstai\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bsta\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstiamo\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstate\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstanno\\b", "stare", text, ignore.case = TRUE)

  text <- gsub("\\bposso\\b", "potere", text, ignore.case = TRUE)
  text <- gsub("\\bpuoi\\b", "potere", text, ignore.case = TRUE)
  text <- gsub("\\bpuo\\b", "potere", text, ignore.case = TRUE)
  text <- gsub("\\bpossiamo\\b", "potere", text, ignore.case = TRUE)
  text <- gsub("\\bpotete\\b", "potere", text, ignore.case = TRUE)
  text <- gsub("\\bpossono\\b", "potere", text, ignore.case = TRUE)

  text <- gsub("\\bvoglio\\b", "volere", text, ignore.case = TRUE)
  text <- gsub("\\bvuoi\\b", "volere", text, ignore.case = TRUE)
  text <- gsub("\\bvuole\\b", "volere", text, ignore.case = TRUE)
  text <- gsub("\\bvogliamo\\b", "volere", text, ignore.case = TRUE)
  text <- gsub("\\bvolete\\b", "volere", text, ignore.case = TRUE)
  text <- gsub("\\bvogliono\\b", "volere", text, ignore.case = TRUE)

  text <- gsub("\\bdevo\\b", "dovere", text, ignore.case = TRUE)
  text <- gsub("\\bdevi\\b", "dovere", text, ignore.case = TRUE)
  text <- gsub("\\bdeve\\b", "dovere", text, ignore.case = TRUE)
  text <- gsub("\\bdobbiamo\\b", "dovere", text, ignore.case = TRUE)
  text <- gsub("\\bdovete\\b", "dovere", text, ignore.case = TRUE)
  text <- gsub("\\bdevono\\b", "dovere", text, ignore.case = TRUE)

  text <- gsub("\\bvedo\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvedi\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvede\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvediamo\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvedete\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvedono\\b", "vedere", text, ignore.case = TRUE)

  text <- gsub("\\bso\\b", "sapere", text, ignore.case = TRUE)
  text <- gsub("\\bsai\\b", "sapere", text, ignore.case = TRUE)
  text <- gsub("\\bsa\\b", "sapere", text, ignore.case = TRUE)
  text <- gsub("\\bsappiamo\\b", "sapere", text, ignore.case = TRUE)
  text <- gsub("\\bsapete\\b", "sapere", text, ignore.case = TRUE)
  text <- gsub("\\bsanno\\b", "sapere", text, ignore.case = TRUE)

  text <- gsub("\\bperda\\b", "prendere", text)
  text <- gsub("\\bpresa\\b", "prendere", text)
  text <- gsub("\\bpreso\\b", "prendere", text)

  # Handle pronoun-attached infinitives (avermi, essermi, etc.) - convert to base infinitive
  # These must come BEFORE individual word conversions
  # "avere" with pronouns
  text <- gsub("\\bavermi\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\baverti\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\baversi\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\baverci\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\bavervi\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\baverlo\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\baverla\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\baverli\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\baverle\\b", "avere", text, ignore.case = TRUE)
  text <- gsub("\\baverne\\b", "avere", text, ignore.case = TRUE)

  # "essere" with pronouns
  text <- gsub("\\bessermi\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\besserti\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\bessersi\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\besserci\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\besservi\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\besserlo\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\besserla\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\besserli\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\besserle\\b", "essere", text, ignore.case = TRUE)
  text <- gsub("\\besserne\\b", "essere", text, ignore.case = TRUE)

  # "fare" with pronouns
  text <- gsub("\\bfarmi\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfarti\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfarsi\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfarci\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfarvi\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfarlo\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfarla\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfarli\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfarle\\b", "fare", text, ignore.case = TRUE)
  text <- gsub("\\bfarne\\b", "fare", text, ignore.case = TRUE)

  # "dire" with pronouns
  text <- gsub("\\bdirmi\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdirti\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdirsi\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdirci\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdirvi\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdirlo\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdirla\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdirli\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdirle\\b", "dire", text, ignore.case = TRUE)
  text <- gsub("\\bdirne\\b", "dire", text, ignore.case = TRUE)

  # "vedere" with pronouns
  text <- gsub("\\bvedermi\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvederti\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvedersi\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvederci\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvedervi\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvederlo\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvederla\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvederli\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvederle\\b", "vedere", text, ignore.case = TRUE)
  text <- gsub("\\bvederne\\b", "vedere", text, ignore.case = TRUE)

  # "andare" with pronouns
  text <- gsub("\\bandarmi\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bandarti\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bandarsi\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bandarci\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bandarvi\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bandarlo\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bandarla\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bandarli\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bandarle\\b", "andare", text, ignore.case = TRUE)
  text <- gsub("\\bandarne\\b", "andare", text, ignore.case = TRUE)

  # "venire" with pronouns
  text <- gsub("\\bvenirmi\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvenirti\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvenirsi\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvenirci\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvenirvi\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvenirlo\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvenirla\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvenirli\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvenirle\\b", "venire", text, ignore.case = TRUE)
  text <- gsub("\\bvenirne\\b", "venire", text, ignore.case = TRUE)

  # "stare" with pronouns
  text <- gsub("\\bstarmi\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstarti\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstarsi\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstarci\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstarvi\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstarlo\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstarla\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstarli\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstarle\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstarne\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstata\\b", "stare", text, ignore.case = TRUE)
  text <- gsub("\\bstato\\b", "stare", text, ignore.case = TRUE)

  # General pattern for any verb ending in -are, -ere, -ire with pronouns
  # This catches verbs not explicitly listed above
  text <- gsub("\\b([a-z]+)armi\\b", "\\1are", text)
  text <- gsub("\\b([a-z]+)arti\\b", "\\1are", text)
  text <- gsub("\\b([a-z]+)arsi\\b", "\\1are", text)
  text <- gsub("\\b([a-z]+)arci\\b", "\\1are", text)
  text <- gsub("\\b([a-z]+)arvi\\b", "\\1are", text)
  text <- gsub("\\b([a-z]+)arlo\\b", "\\1are", text)
  text <- gsub("\\b([a-z]+)arla\\b", "\\1are", text)
  text <- gsub("\\b([a-z]+)arli\\b", "\\1are", text)
  text <- gsub("\\b([a-z]+)arle\\b", "\\1are", text)
  text <- gsub("\\b([a-z]+)arne\\b", "\\1are", text)

  text <- gsub("\\b([a-z]+)ermi\\b", "\\1ere", text)
  text <- gsub("\\b([a-z]+)erti\\b", "\\1ere", text)
  text <- gsub("\\b([a-z]+)ersi\\b", "\\1ere", text)
  text <- gsub("\\b([a-z]+)erci\\b", "\\1ere", text)
  text <- gsub("\\b([a-z]+)ervi\\b", "\\1ere", text)
  text <- gsub("\\b([a-z]+)erlo\\b", "\\1ere", text)
  text <- gsub("\\b([a-z]+)erla\\b", "\\1ere", text)
  text <- gsub("\\b([a-z]+)erli\\b", "\\1ere", text)
  text <- gsub("\\b([a-z]+)erle\\b", "\\1ere", text)
  text <- gsub("\\b([a-z]+)erne\\b", "\\1ere", text)

  text <- gsub("\\b([a-z]+)irmi\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)irti\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)irsi\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)irci\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)irvi\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)irlo\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)irla\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)irli\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)irle\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)irne\\b", "\\1ire", text)

  # Regular verbs - IRE conjugation
  text <- gsub("\\b([a-z]+)isco\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)isci\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)isce\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)iamo\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)ite\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)iscono\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)ito\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)ita\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)iti\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)iro\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)irai\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)ira\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)iremo\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)irete\\b", "\\1ire", text)
  text <- gsub("\\b([a-z]+)iranno\\b", "\\1ire", text)

  text <- gsub("\\bfortunare\\b", "fortunato", text)
  text <- gsub("\\bfortunata\\b", "fortunato", text)
  text <- gsub("\\bquella\\b", "quello", text)
  text <- gsub("\\bquelli\\b", "quello", text)
  text <- gsub("\\bquelle\\b", "quello", text)
  text <- gsub("\\bquesta\\b", "questo", text)
  text <- gsub("\\bquesti\\b", "questo", text)
  text <- gsub("\\bqueste\\b", "questo", text)
  text <- gsub("\\beere\\b", "essere", text)
  text <- gsub("\\ballestero\\b", "estero", text)

  text <- gsub("\\bfatto\\b", "fare", text)
  text <- gsub("\\bsoins\\b", "essere", text)

  # "riuscire" verb
  text <- gsub("\\briesco\\b", "riuscire", text)
  text <- gsub("\\briesca\\b", "riuscire", text)
  text <- gsub("\\briesci\\b", "riuscire", text)
  text <- gsub("\\briesce\\b", "riuscire", text)
  text <- gsub("\\briusciamo\\b", "riuscire", text)
  text <- gsub("\\briusciate\\b", "riuscire", text)
  text <- gsub("\\briescano\\b", "riuscire", text)
  text <- gsub("\\briuscivamo\\b", "riuscire", text)
  text <- gsub("\\briuscivate\\b", "riuscire", text)
  text <- gsub("\\briuscivano\\b", "riuscire", text)
  text <- gsub("\\briuscivo\\b", "riuscire", text)
  text <- gsub("\\briusciva\\b", "riuscire", text)
  text <- gsub("\\briuscito\\b", "riuscire", text)

  # "abbandonare" verb
  text <- gsub("\\babbandono\\b", "abbandonare", text)
  text <- gsub("\\babbandoni\\b", "abbandonare", text)
  text <- gsub("\\babbandona\\b", "abbandonare", text)
  text <- gsub("\\babbandoniamo\\b", "abbandonare", text)
  text <- gsub("\\babbandonate\\b", "abbandonare", text)
  text <- gsub("\\babbandonino\\b", "abbandonare", text)
  text <- gsub("\\babbandonato\\b", "abbandonare", text)
  text <- gsub("\\babbandonata\\b", "abbandonare", text)

  # "accogliere" verb
  text <- gsub("\\baccogli\\b", "accogliere", text)
  text <- gsub("\\baccolgo\\b", "accogliere", text)
  text <- gsub("\\baccoglie\\b", "accogliere", text)
  text <- gsub("\\baccogliamo\\b", "accogliere", text)
  text <- gsub("\\baccogliate\\b", "accogliere", text)
  text <- gsub("\\baccoglino\\b", "accogliere", text)
  text <- gsub("\\baccolto\\b", "accogliere", text)
  text <- gsub("\\baccolta\\b", "accogliere", text)

  # "tramandare" verb
  text <- gsub("\\btramandato\\b", "tramandare", text)
  text <- gsub("\\btramandata\\b", "tramandare", text)
  text <- gsub("\\btramandano\\b", "tramandare", text)
  text <- gsub("\\btramandiamo\\b", "tramandare", text)
  text <- gsub("\\btramandate\\b", "tramandare", text)
  text <- gsub("\\btramandino\\b", "tramandare", text)
  text <- gsub("\\btramando\\b", "tramandare", text)
  text <- gsub("\\btramandi\\b", "tramandare", text)
  text <- gsub("\\btramandato\\b", "tramandare", text)

  # "parlare" verb
  text <- gsub("\\bparlo\\b", "parlare", text)
  text <- gsub("\\bparli\\b", "parlare", text)
  text <- gsub("\\bparla\\b", "parlare", text)
  text <- gsub("\\bparliamo\\b", "parlare", text)
  text <- gsub("\\bparlate\\b", "parlare", text)
  text <- gsub("\\bparlano\\b", "parlare", text)
  text <- gsub("\\bparlino\\b", "parlare", text)
  text <- gsub("\\bparlire\\b", "parlare", text)
  text <- gsub("\\bparlavo\\b", "parlare", text)
  text <- gsub("\\bparlavi\\b", "parlare", text)
  text <- gsub("\\bparlava\\b", "parlare", text)
  text <- gsub("\\bparlavamo\\b", "parlare", text)
  text <- gsub("\\bparlavate\\b", "parlare", text)
  text <- gsub("\\bparlavano\\b", "parlare", text)
  text <- gsub("\\bparlvate\\b", "parlare", text)
  text <- gsub("\\bparlato\\b", "parlare", text)
  text <- gsub("\\bparlata\\b", "parlare", text)

  # "abbandonare" verb
  text <- gsub("\\babbandono\\b", "abbandonare", text)
  text <- gsub("\\babbandoni\\b", "abbandonare", text)
  text <- gsub("\\babbandona\\b", "abbandonare", text)
  text <- gsub("\\babbandoniamo\\b", "abbandonare", text)
  text <- gsub("\\babbandonate\\b", "abbandonare", text)
  text <- gsub("\\babbandonino\\b", "abbandonare", text)
  text <- gsub("\\babbandonato\\b", "abbandonare", text)
  text <- gsub("\\babbandonata\\b", "abbandonare", text)

  # "accogliere" verb
  text <- gsub("\\baccogli\\b", "accogliere", text)
  text <- gsub("\\baccolgo\\b", "accogliere", text)
  text <- gsub("\\baccoglie\\b", "accogliere", text)
  text <- gsub("\\baccogliamo\\b", "accogliere", text)
  text <- gsub("\\baccogliate\\b", "accogliere", text)
  text <- gsub("\\baccoglino\\b", "accogliere", text)
  text <- gsub("\\baccolto\\b", "accogliere", text)
  text <- gsub("\\baccolta\\b", "accogliere", text)

  # "tramandare" verb
  text <- gsub("\\btramandato\\b", "tramandare", text)
  text <- gsub("\\btramandata\\b", "tramandare", text)
  text <- gsub("\\btramandano\\b", "tramandare", text)
  text <- gsub("\\btramandiamo\\b", "tramandare", text)
  text <- gsub("\\btramandate\\b", "tramandare", text)
  text <- gsub("\\btramandino\\b", "tramandare", text)
  text <- gsub("\\btramando\\b", "tramandare", text)
  text <- gsub("\\btramandi\\b", "tramandare", text)
  text <- gsub("\\btramandato\\b", "tramandare", text)

  # "parlare" verb
  text <- gsub("\\bparlo\\b", "parlare", text)
  text <- gsub("\\bparli\\b", "parlare", text)
  text <- gsub("\\bparla\\b", "parlare", text)
  text <- gsub("\\bparliamo\\b", "parlare", text)
  text <- gsub("\\bparlate\\b", "parlare", text)
  text <- gsub("\\bparlano\\b", "parlare", text)
  text <- gsub("\\bparlino\\b", "parlare", text)
  text <- gsub("\\bparlire\\b", "parlare", text)
  text <- gsub("\\bparlavo\\b", "parlare", text)
  text <- gsub("\\bparlavi\\b", "parlare", text)
  text <- gsub("\\bparlava\\b", "parlare", text)
  text <- gsub("\\bparlavamo\\b", "parlare", text)
  text <- gsub("\\bparlavate\\b", "parlare", text)
  text <- gsub("\\bparlavano\\b", "parlare", text)
  text <- gsub("\\bparlvate\\b", "parlare", text)
  text <- gsub("\\bparlato\\b", "parlare", text)
  text <- gsub("\\bparlata\\b", "parlare", text)

  # "abbandonare" verb
  text <- gsub("\\babbandonati\\b", "abbandonare", text)
  text <- gsub("\\babbandonire\\b", "abbandonare", text)

  # "abbassare" verb
  text <- gsub("\\babbassa\\b", "abbassare", text)
  text <- gsub("\\babbassando\\b", "abbassare", text)

  # "abitare" verb
  text <- gsub("\\babitano\\b", "abitare", text)
  text <- gsub("\\babitando\\b", "abitare", text)
  text <- gsub("\\babitava\\b", "abitare", text)
  text <- gsub("\\babitavano\\b", "abitare", text)
  text <- gsub("\\babitire\\b", "abitare", text)
  text <- gsub("\\babire\\b", "abitare", text)

  # "abitudini" noun
  text <- gsub("\\babituata\\b", "abitudine", text)
  text <- gsub("\\babitudinale\\b", "abitudine", text)
  text <- gsub("\\babituale\\b", "abitudine", text)

  # "accettare" verb
  text <- gsub("\\baccetta\\b", "accettare", text)
  text <- gsub("\\baccettata\\b", "accettare", text)

  # "accennare" verb
  text <- gsub("\\baccennavo\\b", "accennare", text)

  # "accoglienza" noun (derived from "accogliere")
  text <- gsub("\\baccoglienza\\b", "accogliere", text)

  # "accorgere" verb
  text <- gsub("\\baccorgendo\\b", "accorgere", text)
  text <- gsub("\\baccorto\\b", "accorgere", text)
  text <- gsub("\\baccorte\\b", "accorgere", text)
  text <- gsub("\\baccorti\\b", "accorgere", text)

  # "acqua" noun (water)
  text <- gsub("\\bacquatica\\b", "acqua", text)
  text <- gsub("\\blacqua\\b", "acqua", text)

  # "acquisire" verb
  text <- gsub("\\bacquisterebbe\\b", "acquisire", text)
  text <- gsub("\\bacquisto\\b", "acquisire", text)

  # "adattare" verb
  text <- gsub("\\badatta\\b", "adattare", text)
  text <- gsub("\\badattire\\b", "adattare", text)

  # "adeguare" verb
  text <- gsub("\\badeguata\\b", "adeguare", text)
  text <- gsub("\\badeguato\\b", "adeguare", text)
  text <- gsub("\\badeguando\\b", "adeguare", text)

  # "adorare" verb
  text <- gsub("\\badoravano\\b", "adorare", text)

  # "affascinante" adverb
  text <- gsub("\\baffascina\\b", "affascinante", text)
  text <- gsub("\\baffascinata\\b", "affascinante", text)
  text <- gsub("\\baffascinato\\b", "affascinante", text)
  text <- gsub("\\baffascinati\\b", "affascinante", text)

  # "affetto" noun
  text <- gsub("\\baffettivi\\b", "affetto", text)
  text <- gsub("\\baffettivo\\b", "affetto", text)
  text <- gsub("\\baffezzionati\\b", "affetto", text)
  text <- gsub("\\baffezionato\\b", "affetto", text)

  # "affittare" verb
  text <- gsub("\\baffittando\\b", "affittare", text)
  text <- gsub("\\baffittato\\b", "affittare", text)
  text <- gsub("\\baffittavano\\b", "affittare", text)
  text <- gsub("\\baffitto\\b", "affittare", text)

  # "affrontare" verb
  text <- gsub("\\baffrontando\\b", "affrontare", text)
  text <- gsub("\\baffrontate\\b", "affrontare", text)
  text <- gsub("\\baffrontiamoli\\b", "affrontare", text)

  # "africa" country
  text <- gsub("\\bafricana\\b", "africa", text)
  text <- gsub("\\bafricane\\b", "africa", text)

  # "alcuni" ("alcune")
  text <- gsub("\\balcune\\b", "alcuni", text)

  # "interno" ("all'interno")
  text <- gsub("\\ballinterno\\b", "interno", text)

  # "alta" ("alto")
  text <- gsub("\\balta\\b", "alto", text)

  # "altro" ("altra", "altre", "altri")
  text <- gsub("\\baltra\\b", "altro", text)
  text <- gsub("\\baltre\\b", "altro", text)
  text <- gsub("\\baltri\\b", "altro", text)

  # "andare" ("andata", "andava")
  text <- gsub("\\bandata\\b", "andare", text)
  text <- gsub("\\bandava\\b", "andare", text)

  # "anno" ("anni")
  text <- gsub("\\banni\\b", "anno", text)

  # "arrivare" ("arriva", "arrivano", "arrivare")
  text <- gsub("\\barriva\\b", "arrivare", text)
  text <- gsub("\\barrivano\\b", "arrivare", text)

  # "bassa" ("basso")
  text <- gsub("\\bbassa\\b", "basso", text)

  # "bello" ("bella")
  text <- gsub("\\bbella\\b", "bello", text)

  # "bisogno" ("bisogna", "bisognerebbe")
  text <- gsub("\\bbisogna\\b", "bisogno", text)
  text <- gsub("\\bbisognerebbe\\b", "bisogno", text)

  # "cambiare" ("cambia", "cambiamenti", "cambiamento", "cambiata", "cambiato")
  text <- gsub("\\bcambia\\b", "cambiare", text)
  text <- gsub("\\bcambiamenti\\b", "cambiare", text)
  text <- gsub("\\bcambiamento\\b", "cambiare", text)
  text <- gsub("\\bcambiata\\b", "cambiare", text)
  text <- gsub("\\bcambiato\\b", "cambiare", text)

  # "castello" ("castelli")
  text <- gsub("\\bcastelli\\b", "castello", text)

  # "comune" ("comui", "comunire", "comunale")
  text <- gsub("\\bcomui\\b", "comune", text)
  text <- gsub("\\bcomunire\\b", "comune", text)
  text <- gsub("\\bcomunale\\b", "comune", text)

  # "confine" ("confini")
  text <- gsub("\\bconfini\\b", "confine", text)

  # "conoscenza" ("conoscere", "conosco")
  text <- gsub("\\bconoscere\\b", "conoscenza", text)
  text <- gsub("\\bconosco\\b", "conoscenza", text)

  # "cosa" ("cose", "cosi")
  text <- gsub("\\bcose\\b", "cosa", text)
  text <- gsub("\\bcosi\\b", "cosa", text)

  # "coltura" ("colturale", "colturali")
  text <- gsub("\\bcolturale\\b", "coltura", text)
  text <- gsub("\\bcolturali\\b", "coltura", text)

  # "aosta" ("d'aosta")
  text <- gsub("\\bdaosta\\b", "aosta", text)

  # differente (differenza)
  text <- gsub("\\bdifferenza\\b", "differente", text)

  # dire (dicevo, direi)
  text <- gsub("\\bdicevo\\b", "dire", text)
  text <- gsub("\\bdirei\\b", "dire", text)

  # dovere (divere)
  text <- gsub("\\bdivere\\b", "dovere", text)

  # diverso (diverse)
  text <- gsub("\\bdiverse\\b", "diverso", text)

  # facile (facilmente)
  text <- gsub("\\bfacilmente\\b", "facile", text)

  # famiglia (damiglie)
  text <- gsub("\\bdamiglie\\b", "famiglia", text)

  # fare (far, fatta)
  text <- gsub("\\bfar\\b", "fare", text)
  text <- gsub("\\bfatta\\b", "fare", text)

  # forza (forte)
  text <- gsub("\\bforte\\b", "forza", text)

  # giorno (giorni, giornata)
  text <- gsub("\\bgiorni\\b", "giorno", text)
  text <- gsub("\\bgiornata\\b", "giorno", text)

  # guardare (gire, guardo)
  text <- gsub("\\bgire\\b", "guardare", text)
  text <- gsub("\\bguardo\\b", "guardare", text)

  # importante (importanti)
  text <- gsub("\\bimportanti\\b", "importante", text)

  # localita (localire)
  text <- gsub("\\blocalire\\b", "localita", text)

  # legame (legata, legate, legati, legato)
  text <- gsub("\\blegata\\b", "legame", text)
  text <- gsub("\\blegate\\b", "legame", text)
  text <- gsub("\\blegati\\b", "legame", text)
  text <- gsub("\\blegato\\b", "legame", text)

  # avere (lho)
  text <- gsub("\\blho\\b", "avere", text)

  # luogo (luoghi)
  text <- gsub("\\bluoghi\\b", "luogo", text)

  # uomo (luomo)
  text <- gsub("\\bluomo\\b", "uomo", text)

  # mancanza (manca)
  text <- gsub("\\bmanca\\b", "mancanza", text)

  # molto (molta, molti, molte)
  text <- gsub("\\bmolta\\b", "molto", text)
  text <- gsub("\\bmolti\\b", "molto", text)
  text <- gsub("\\bmolte\\b", "molto", text)

  # montagna (mont, monte, montagne, montjovet)
  text <- gsub("\\bmont\\b", "montagna", text)
  text <- gsub("\\bmonte\\b", "montagna", text)
  text <- gsub("\\bmontagne\\b", "montagna", text)
  text <- gsub("\\bmontjovet\\b", "montagna", text)

  # peculiare (pecularire)
  text <- gsub("\\bpecularire\\b", "peculiare", text)

  # pensare (penso)
  text <- gsub("\\bpenso\\b", "pensare", text)

  # percezione (percepire)
  text <- gsub("\\bpercepire\\b", "percezione", text)

  # piccolo (piccoli, piccola, piccole)
  text <- gsub("\\bpiccoli\\b", "piccolo", text)
  text <- gsub("\\bpiccola\\b", "piccolo", text)
  text <- gsub("\\bpiccole\\b", "piccolo", text)

  # portare (porta)
  text <- gsub("\\bporta\\b", "portare", text)

  # problema (problemi)
  text <- gsub("\\bproblemi\\b", "problema", text)

  # quale (quali)
  text <- gsub("\\bquali\\b", "quale", text)

  # quello (quel, quelli, quelle, quella)
  text <- gsub("\\bquel\\b", "quello", text)
  text <- gsub("\\bquelli\\b", "quello", text)
  text <- gsub("\\bquelle\\b", "quello", text)
  text <- gsub("\\bquella\\b", "quello", text)

  # questo (quest, questi, queste, questa)
  text <- gsub("\\bquest\\b", "questo", text)
  text <- gsub("\\bquesti\\b", "questo", text)
  text <- gsub("\\bqueste\\b", "questo", text)
  text <- gsub("\\bquesta\\b", "questo", text)

  # ripetere (ripeto)
  text <- gsub("\\bripeto\\b", "ripetere", text)

  # sentire (sento)
  text <- gsub("\\bsento\\b", "sentire", text)

  # significato (significa)
  text <- gsub("\\bsignifica\\b", "significato", text)

  # tanto (tanti, tante, tanta, tantissimo, tantissimi, tantissime)
  text <- gsub("\\btanti\\b", "tanto", text)
  text <- gsub("\\btante\\b", "tanto", text)
  text <- gsub("\\btanta\\b", "tanto", text)
  text <- gsub("\\btantissimo\\b", "tanto", text)
  text <- gsub("\\btantissimi\\b", "tanto", text)
  text <- gsub("\\btantissime\\b", "tanto", text)

  # turismo (turista, turisti, turiste, turistico, turistica, turistiche)
  text <- gsub("\\bturista\\b", "turismo", text)
  text <- gsub("\\bturisti\\b", "turismo", text)
  text <- gsub("\\bturiste\\b", "turismo", text)
  text <- gsub("\\bturistico\\b", "turismo", text)
  text <- gsub("\\bturistica\\b", "turismo", text)
  text <- gsub("\\bturistiche\\b", "turismo", text)

  # valle (vallata, vallate, valli, val)
  text <- gsub("\\bvallata\\b", "valle", text)
  text <- gsub("\\bvallate\\b", "valle", text)
  text <- gsub("\\bvalli\\b", "valle", text)
  text <- gsub("\\bval\\b", "valle", text)

  # vivere (vivo, vive, vissuto, vire)
  text <- gsub("\\bvivo\\b", "vivere", text)
  text <- gsub("\\bvive\\b", "vivere", text)
  text <- gsub("\\bvissuto\\b", "vivere", text)
  text <- gsub("\\bvire\\b", "vivere", text)

  # volere (vuol)
  text <- gsub("\\bvuol\\b", "volere", text)

  # volta (volte)
  text <- gsub("\\bvolte\\b", "volta", text)

  # zona (zone)
  text <- gsub("\\bzone\\b", "zona", text)

  # altro (laltro)
  text <- gsub("\\blaltro\\b", "altro", text)

  # poco (po, pochi, pochino)
  text <- gsub("\\bpo\\b", "poco", text)
  text <- gsub("\\bpochi\\b", "poco", text)
  text <- gsub("\\bpochino\\b", "poco", text)

  # promuovere (promozione)
  text <- gsub("\\bpromozione\\b", "promuovere", text)

  # proprio (propria)
  text <- gsub("\\bpropria\\b", "proprio", text)

  # rimanere (rimane)
  text <- gsub("\\brimane\\b", "rimanere", text)

  # sapere (saprei)
  text <- gsub("\\bsaprei\\b", "sapere", text)

  # trovare (trovo)
  text <- gsub("\\btrovo\\b", "trovare", text)

  # solito (solire)
  text <- gsub("\\bsolire\\b", "solito", text)

  # valore (valori)
  text <- gsub("\\bvalori\\b", "valore", text)

  # termine (termini)
  text <- gsub("\\btermini\\b", "termine", text)

  # vedere (visto, vista)
  text <- gsub("\\bvisto\\b", "vedere", text)
  text <- gsub("\\bvista\\b", "vedere", text)

  # dire (detto)
  text <- gsub("\\bdetto\\b", "dire", text)

  # era (cera, cerano)
  text <- gsub("\\bcerano\\b", "cera", text)

  # cercare (cercando)
  text <- gsub("\\bcercando\\b", "cercare", text)
  # territorio (territori)
  text <- gsub("\\bterritori\\b", "territorio", text)

  # stesso (stessa)
  text <- gsub("\\bterritori\\b", "territorio", text)

  # tutto (tutta, tutti, tutte)
  text <- gsub("\\btutti\\b", "tutto", text)
  text <- gsub("\\btutta\\b", "tutto", text)
  text <- gsub("\\btutte\\b", "tutto", text)

  # potere (poter, potrebbe)
  text <- gsub("\\bpotrebbe\\b", "potere", text)
  text <- gsub("\\bpoter\\b", "potere", text)

  # pubblicita (pubblicire)
  text <- gsub("\\bpubblicire\\b", "pubblicita", text)

  # punto (punti)
  text <- gsub("\\bpunti\\b", "punto", text)

  # primo (prima)
  text <- gsub("\\bprima\\b", "prima", text)

  # qualita (qualire)
  text <- gsub("\\bqualire\\b", "qualita", text)

  # altro (unaltra)
  text <- gsub("\\bunaltra\\b", "altro", text)

  # albergo (alberghi)
  text <- gsub("\\balberghi\\b", "albergo", text)
  # arco (larco)
  text <- gsub("\\blarco\\b", "arco", text)
  # inizio (allinizio)
  text <- gsub("\\ballinizio\\b", "inizio", text)
  # andare (andando, andati, andavano)
  text <- gsub("\\bandando\\b", "andare", text)
  text <- gsub("\\bandati\\b", "andare", text)
  text <- gsub("\\bandavano\\b", "andare", text)
  # aprire (aperto)
  text <- gsub("\\baperto\\b", "aprire", text)
  # area (aree)
  text <- gsub("\\baree\\b", "area", text)
  text <- gsub("\\bunarea\\b", "area", text)
  # arrivare (arrivata, arrivati, arrivava, arrivo, arrivi)
  text <- gsub("\\barrivata\\b", "arrivare", text)
  text <- gsub("\\barrivati\\b", "arrivare", text)
  text <- gsub("\\barrivava\\b", "arrivare", text)
  text <- gsub("\\barrivo\\b", "arrivare", text)
  text <- gsub("\\barrivi\\b", "arrivare", text)
  # aspetto (aspetti, laspetto)
  text <- gsub("\\baspetti\\b", "aspetto", text)
  text <- gsub("\\blaspetto\\b", "aspetto", text)
  # avere (aver, lha)
  text <- gsub("\\baver\\b", "avere", text)
  text <- gsub("\\blha\\b", "avere", text)
  # bellezza (bello, belle)
  text <- gsub("\\bbello\\b", "bellezza", text)
  text <- gsub("\\bbelle\\b", "bellezza", text)
  # bene (ben)
  text <- gsub("\\bben\\b", "bene", text)
  # caratteristica (caratteristiche)
  text <- gsub("\\bcaratteristiche\\b", "caratteristica", text)
  # casa (case)
  text <- gsub("\\bcase\\b", "casa", text)
  # cercare (cerco)
  text <- gsub("\\bcerco\\b", "cercare", text)
  # cera (cere)
  text <- gsub("\\bcere\\b", "cera", text)
  # chiudere (chiusa, chiuso)
  text <- gsub("\\bchiusa\\b", "chiudere", text)
  text <- gsub("\\bchiuso\\b", "chiudere", text)
  # comune (comuni)
  text <- gsub("\\bcomuni\\b", "comune", text)
  # conoscenza (conoscenze)
  text <- gsub("\\bconoscenze\\b", "conoscenza", text)
  # conoscere (conosciuto, conosciuta)
  text <- gsub("\\bconosciuto\\b", "conoscere", text)
  text <- gsub("\\bconosciuta\\b", "conoscere", text)
  # continuita (continuo, continuano, continua, continuare, continuire)
  text <- gsub("\\bcontinuo\\b", "continuita", text)
  text <- gsub("\\bcontinuano\\b", "continuita", text)
  text <- gsub("\\bcontinua\\b", "continuita", text)
  text <- gsub("\\bcontinuare\\b", "continuita", text)
  text <- gsub("\\bcontinuire\\b", "continuita", text)
  # crescita (crescire)
  text <- gsub("\\bcrescire\\b", "crescita", text)
  # cultura (culturale, culturali)
  text <- gsub("\\bculturale\\b", "cultura", text)
  text <- gsub("\\bculturali\\b", "cultura", text)
  # ayas (dayas)
  text <- gsub("\\bdayas\\b", "ayas", text)
  # anno (dellanno, lanno)
  text <- gsub("\\bdellanno\\b", "anno", text)
  text <- gsub("\\blanno\\b", "anno", text)
  # est (dellest)
  text <- gsub("\\bdellest\\b", "est", text)
  # estate (destate)
  text <- gsub("\\bdestate\\b", "estate", text)
  # determinare (determinate)
  text <- gsub("\\bdeterminate\\b", "determinare", text)
  # dire (diceva, dicevano)
  text <- gsub("\\bdiceva\\b", "dire", text)
  text <- gsub("\\bdicevano\\b", "dire", text)
  # differenza (differenze, differenzia, differente)
  text <- gsub("\\bdifferenze\\b", "differenza", text)
  text <- gsub("\\bdifferenzia\\b", "differenza", text)
  text <- gsub("\\bdifferente\\b", "differenza", text)
  # diventare (diventa, diventano)
  text <- gsub("\\bdiventa\\b", "diventare", text)
  text <- gsub("\\bdiventano\\b", "diventare", text)
  # diverso (diversa, diversi, diverse)
  text <- gsub("\\bdiversa\\b", "diverso", text)
  text <- gsub("\\bdiversi\\b", "diverso", text)
  text <- gsub("\\bdiverse\\b", "diverso", text)
  # dovere (dovrebbe)
  text <- gsub("\\bdovrebbe\\b", "dovere", text)
  # enorme (enormi)
  text <- gsub("\\benormi\\b", "enorme", text)
  # eventi (evento)
  text <- gsub("\\bevento\\b", "eventi", text)
  # famiglia (famiglie)
  text <- gsub("\\bfamiglie\\b", "famiglia", text)
  # festa (feste)
  text <- gsub("\\bfeste\\b", "festa", text)
  # fine (finire)
  text <- gsub("\\bfinire\\b", "fine", text)
  # forza (forti)
  text <- gsub("\\bforti\\b", "forza", text)
  # gestione (gestire)
  text <- gsub("\\bgestire\\b", "gestione", text)
  # giovani (giovane)
  text <- gsub("\\bgiovane\\b", "giovani", text)
  # grande (grandi)
  text <- gsub("\\bgrandi\\b", "grande", text)
  # grosso (grossa, grossi, grosse)
  text <- gsub("\\bgrossa\\b", "grosso", text)
  text <- gsub("\\bgrossi\\b", "grosso", text)
  text <- gsub("\\bgrosse\\b", "grosso", text)
  # guardare (guarda, guardi)
  text <- gsub("\\bguarda\\b", "guardare", text)
  text <- gsub("\\bguardi\\b", "guardare", text)
  # inteso (intendo, intendere)
  text <- gsub("\\bintendo\\b", "inteso", text)
  text <- gsub("\\bintendere\\b", "inteso", text)
  # interesse (interessante)
  text <- gsub("\\binteressante\\b", "interesse", text)
  # francia (francese)
  text <- gsub("\\bfrancese\\b", "francia", text)
  # italia (italiani, italiano, litalia)
  text <- gsub("\\bitaliani\\b", "italia", text)
  text <- gsub("\\bitaliano\\b", "italia", text)
  text <- gsub("\\blitalia\\b", "italia", text)
  # alta (lalta)
  text <- gsub("\\blalta\\b", "alta", text)
  # autostrada (lautostrada)
  text <- gsub("\\blautostrada\\b", "autostrada", text)
  # lavoro (lavorare, lavorato, lavori)
  text <- gsub("\\blavorare\\b", "lavoro", text)
  text <- gsub("\\blavorato\\b", "lavoro", text)
  text <- gsub("\\blavori\\b", "lavoro", text)
  # avvento (lavvento)
  text <- gsub("\\blavvento\\b", "avvento", text)
  # leggere (letto)
  text <- gsub("\\bletto\\b", "leggere", text)
  # inverno (linverno)
  text <- gsub("\\blinverno\\b", "inverno", text)
  # localita
  # no replacement needed if no variants
  # locale (locali)
  text <- gsub("\\blocali\\b", "locale", text)
  # maggiore (maggior)
  text <- gsub("\\bmaggior\\b", "maggiore", text)
  # mentalita (mentalire, mente)
  text <- gsub("\\bmentalire\\b", "mentalita", text)
  text <- gsub("\\bmente\\b", "mentalita", text)
  # mettere (messo)
  text <- gsub("\\bmesso\\b", "mettere", text)
  # mezzo (mezza)
  text <- gsub("\\bmezza\\b", "mezzo", text)
  # momenti (momento)
  text <- gsub("\\bmomento\\b", "momenti", text)
  # motivo (motivi)
  text <- gsub("\\bmotivi\\b", "motivo", text)
  # natura (naturale, naturali)
  text <- gsub("\\bnaturale\\b", "natura", text)
  text <- gsub("\\bnaturali\\b", "natura", text)
  # negativo (negativa)
  text <- gsub("\\bnegativa\\b", "negativo", text)
  # nessuno (nessun)
  text <- gsub("\\bnessun\\b", "nessuno", text)
  # opportunita (opportunire)
  text <- gsub("\\bopportunire\\b", "opportunita", text)
  # organizzare (organizza)
  text <- gsub("\\borganizza\\b", "organizzare", text)
  # ospitare (ospire)
  text <- gsub("\\bospire\\b", "ospitare", text)
  # paese (paesi)
  text <- gsub("\\bpaesi\\b", "paese", text)
  # parlare (parlando)
  text <- gsub("\\bparlando\\b", "parlare", text)
  # particolarita (particolarire, particolare)
  text <- gsub("\\bparticolarire\\b", "particolarita", text)
  text <- gsub("\\bparticolare\\b", "particolarita", text)
  # passare (passa)
  text <- gsub("\\bpassa\\b", "passare", text)
  # peculiarita (peculiarire)
  text <- gsub("\\bpeculiarire\\b", "peculiarita", text)
  # pensiero (pensa, pensare, pensato)
  text <- gsub("\\bpensa\\b", "pensiero", text)
  text <- gsub("\\bpensare\\b", "pensiero", text)
  text <- gsub("\\bpensato\\b", "pensiero", text)
  # percorso (percorsi)
  text <- gsub("\\bpercorsi\\b", "percorso", text)
  # persone (persona)
  text <- gsub("\\bpersona\\b", "persone", text)
  # politica (politico)
  text <- gsub("\\bpolitico\\b", "politica", text)
  # portare (portano)
  text <- gsub("\\bportano\\b", "portare", text)
  # potere (possa, potrebbero, potrei)
  text <- gsub("\\bpossa\\b", "potere", text)
  text <- gsub("\\bpotrebbero\\b", "potere", text)
  text <- gsub("\\bpotrei\\b", "potere", text)
  # posti (posto)
  text <- gsub("\\bposto\\b", "posti", text)
  # potenzialita
  # no replacement needed if no variants
  # presente (presenze)
  text <- gsub("\\bpresenze\\b", "presente", text)
  # principale (principali)
  text <- gsub("\\bprincipali\\b", "principale", text)
  # problemi (problema, problematiche)
  text <- gsub("\\bproblema\\b", "problemi", text)
  text <- gsub("\\bproblematiche\\b", "problemi", text)
  # prodotti (prodotto)
  text <- gsub("\\bprodotto\\b", "prodotti", text)
  # pubblicizzare (pubblicita, promuovere)
  text <- gsub("\\bpubblicita\\b", "pubblicizzare", text)
  text <- gsub("\\bpromuovere\\b", "pubblicizzare", text)
  # raccontare (raccontato)
  text <- gsub("\\braccontato\\b", "raccontare", text)
  # regione (regionale, regioni)
  text <- gsub("\\bregionale\\b", "regione", text)
  text <- gsub("\\bregioni\\b", "regione", text)
  # rendere (rende)
  text <- gsub("\\brende\\b", "rendere", text)
  # riuscire (riescono)
  text <- gsub("\\briescono\\b", "riuscire", text)
  # attrattiva (unattrattiva)
  text <- gsub("\\bunattrattiva\\b", "attrattiva", text)
  # rimanere (rimangono)
  text <- gsub("\\brimangono\\b", "rimanere", text)
  # risposta (rispondere)
  text <- gsub("\\brispondere\\b", "risposta", text)
  # ritenere (ritengo)
  text <- gsub("\\britengo\\b", "ritenere", text)
  # salire (salgo)
  text <- gsub("\\bsalgo\\b", "salire", text)
  # essere (sara)
  text <- gsub("\\bsara\\b", "essere", text)
  # scuola (scuole)
  text <- gsub("\\bscuole\\b", "scuola", text)
  # seconda (secondo)
  text <- gsub("\\bsecondo\\b", "seconda", text)
  # semplice (semplicemente)
  text <- gsub("\\bsemplicemente\\b", "semplice", text)
  # sentire (sentono)
  text <- gsub("\\bsentono\\b", "sentire", text)
  # sindaco (sindaci)
  text <- gsub("\\bsindaci\\b", "sindaco", text)
  # solo (soltanto)
  text <- gsub("\\bsoltanto\\b", "solo", text)
  # strutture (struttura)
  text <- gsub("\\bstruttura\\b", "strutture", text)
  # spazi (spazio)
  text <- gsub("\\bspazio\\b", "spazi", text)
  # speranza (spero)
  text <- gsub("\\bspero\\b", "speranza", text)
  # stesso (stessa)
  text <- gsub("\\bstessa\\b", "stesso", text)
  # storia (storici, storico)
  text <- gsub("\\bstorici\\b", "storia", text)
  text <- gsub("\\bstorico\\b", "storia", text)
  # strade (strada)
  text <- gsub("\\bstrada\\b", "strade", text)
  # succedere (succede, successo)
  text <- gsub("\\bsuccede\\b", "succedere", text)
  text <- gsub("\\bsuccesso\\b", "succedere", text)
  # sviluppo (sviluppare)
  text <- gsub("\\bsviluppare\\b", "sviluppo", text)
  # tempo (tempi)
  text <- gsub("\\btempi\\b", "tempo", text)
  # territorio (terra)
  text <- gsub("\\bterra\\b", "territorio", text)
  # tradizioni (tradizione)
  text <- gsub("\\btradizione\\b", "tradizioni", text)
  # trovare (trovano, trovato)
  text <- gsub("\\btrovano\\b", "trovare", text)
  text <- gsub("\\btrovato\\b", "trovare", text)
  # turismo (turistici)
  text <- gsub("\\bturistici\\b", "turismo", text)
  # ora (unora)
  text <- gsub("\\bunora\\b", "ora", text)
  # valore (vale, valorizzare)
  text <- gsub("\\bvale\\b", "valore", text)
  text <- gsub("\\bvalorizzare\\b", "valore", text)
  # varie (vari)
  text <- gsub("\\bvari\\b", "varie", text)
  # vicini (vicino)
  text <- gsub("\\bvicino\\b", "vicini", text)
  # vivere (vivono)
  text <- gsub("\\bvivono\\b", "vivere", text)
  # volontariato (volontari)
  text <- gsub("\\bvolontari\\b", "volontariato", text)
  # volere (voglia)
  text <- gsub("\\bvoglia\\b", "volere", text)
  # rischiare (rischia)
  text <- gsub("\\brischia\\b", "rischiare", text)

  # STEP 5: Use tm and SnowballC for stemming
  corpus_temp <- VCorpus(VectorSource(text))
  corpus_temp <- tm_map(corpus_temp, removeWords, stopwords("italian"))
  # corpus_temp <- tm_map(corpus_temp, stemDocument, language = "italian")
  text <- sapply(corpus_temp, as.character)

  # STEP 6: Remove stopwords
  custom_stops <- c("essere", "avere", "fare", "ciao",
                    "per", "in", "di", "a", "da", "in", "con", "su",
                    "tra", "fra", "certamente", "si", "no", "grazie",
                    "ringrazio", "certo", "poi", "eh", "quando", "cui",
                    "quindi", "perche", "che", "mi", "ti", "io","ma",
                    "questo", "quello", "qui", "percio", "me", "te",
                    "con", "ce", "ancora", "magari", "ah", "beh", "ok", "cio",
                    "forse", "ecco", "cosa", "eccetera", "gia", "invece",
                    "insomma", "mah", "neanche", "oppure", "pero", "quello",
                    "questo", "vabbe", "allora", "cioe", "quantaltro",
                    "qualsiasi", "qualcosa", "qualche", "piu", "sicuramente",
                    "assolutamente", "chiaramente", "chiaro", "completamente",
                    "comunque", "de", "due", "tre", "effettivamente", "fin", "fino",
                    "intanto", "lha", "mentre", "mila", "mille", "molto",
                    "naturalmente", "niente", "nonostante", "nulla", "ogni", "ognuno",
                    "ormai", "ovviamente", "pare", "pochettino", "poco",
                    "praticamente", "prima", "primo", "primi", "probabilmente",
                    "proprio", "pur", "purtroppo", "qualunque", "qualcuno",
                    "sembra","sempre", "sicuramente", "soprattutto", "spesso",
                    "talmente", "tendenzialmente", "totalmente", "tre",
                    "ultimamente", "venti", "veramente", "virgolette")
  for(stopword in custom_stops) {
    text <- gsub(paste0("\\b", stopword, "\\b"), "", text)
  }

  # Clean up extra spaces
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)

  # Store result
  inter$processed_final[i] <- text

  if(i %% 10 == 0) {
    cat("Processed", i, "of", nrow(inter), "rows...\n")
  }
}

cat("Processing complete!\n\n")

# Show examples
cat("=== Processed Examples (First 3 rows) ===\n")
for(i in 1:min(3, nrow(inter))) {
  cat("\n--- Row", i, "---\n")
  cat("Original:", substr(inter[i, 2], 1, 100), "...\n")
  cat("Processed:", substr(inter$processed_final[i], 1, 100), "...\n")
}

# View results
head(inter[, c("File.ID", "processed_final")])

# STEP 7: Remove rows with 1 or 2 words in processed_final
cat("\n=== Removing rows with 1 or 2 words ===\n")
# Count words in each processed_final entry
word_counts <- sapply(inter$processed_final, function(x) {
  if(is.na(x) || x == "" || trimws(x) == "") {
    return(0)
  }
  words <- strsplit(trimws(x), "\\s+")[[1]]
  words <- words[words != ""]  # Remove empty strings
  return(length(words))
})

# Remove rows with 1 or 2 words
rows_to_remove <- which(word_counts <= 2)
cat("Removing", length(rows_to_remove), "rows with 1 or 2 words\n")
inter <- inter[-rows_to_remove, ]

cat("Remaining rows:", nrow(inter), "\n")

# STEP 8: Create DTM aggregating by File.ID
cat("\n=== Creating DTM aggregated by File.ID ===\n")

# Aggregate processed_final by File.ID (combine all text for each File.ID)
inter_aggregated <- inter %>%
  group_by(File.ID) %>%
  summarise(
    processed_final = paste(processed_final, collapse = " "),
    .groups = "drop"
  )

cat("Aggregated to", nrow(inter_aggregated), "unique File.IDs\n")

# Clean aggregated text: remove apostrophes at the beginning of words
cat("Cleaning apostrophes and other artifacts...\n")
inter_aggregated$processed_final <- gsub("\\b'([a-z]+)\\b", "\\1", inter_aggregated$processed_final)  # Remove apostrophe at word start
inter_aggregated$processed_final <- gsub("\\b'\\b", "", inter_aggregated$processed_final)  # Remove standalone apostrophes
inter_aggregated$processed_final <- gsub("'", "", inter_aggregated$processed_final)  # Remove any remaining apostrophes
inter_aggregated$processed_final <- gsub("\\s+", " ", inter_aggregated$processed_final)  # Clean up extra spaces
inter_aggregated$processed_final <- trimws(inter_aggregated$processed_final)  # Trim whitespace

# Create corpus from aggregated text
corpus_aggregated <- VCorpus(VectorSource(inter_aggregated$processed_final))


# Create Document-Term Matrix
dtm_aggregated <- DocumentTermMatrix(corpus_aggregated,
                                     control = list(
                                       tolower = FALSE,  # Already lowercase
                                       removePunctuation = FALSE,  # Already removed
                                       removeNumbers = FALSE,  # Already removed
                                       stopwords = FALSE,  # Already removed
                                       stemming = FALSE,  # Already stemmed
                                       wordLengths = c(2, Inf)  # Minimum word length
                                     ))

# Set row names to File.ID
rownames(dtm_aggregated) <- inter_aggregated$File.ID

# Remove terms that start with apostrophe or other problematic characters
cat("Removing terms starting with apostrophes or invalid characters...\n")
terms_to_remove <- grep("^'", colnames(dtm_aggregated), value = TRUE)
if(length(terms_to_remove) > 0) {
  cat("Removing", length(terms_to_remove), "terms starting with apostrophe\n")
  dtm_aggregated <- dtm_aggregated[, !colnames(dtm_aggregated) %in% terms_to_remove]
}

# Also remove terms that don't start with a letter (backup cleanup)
invalid_terms <- grep("^[^a-z]", colnames(dtm_aggregated), value = TRUE)
if(length(invalid_terms) > 0) {
  cat("Removing", length(invalid_terms), "terms with invalid starting characters\n")
  dtm_aggregated <- dtm_aggregated[, !colnames(dtm_aggregated) %in% invalid_terms]
}

dtm <- as.matrix(dtm_aggregated)

dtm <- dtm[,colSums(dtm)>5]

V <- ncol(dtm)
D <- nrow(dtm) # corrisponde a File.ID (o Speaker)

# Division in training and test set
# to have a fair evaluation, we need to divide the data into training and test set such that
# the training and the test are created dividing the cells of each document in two parts:
# not mandatory in equal parts, but it is a good practice
set.seed(525)
split_dtm <- function(dtm, train_prop = 0.8) {
  train_mat <- dtm
  test_mat <- dtm
  for (i in 1:nrow(dtm)) {
    for (j in 1:ncol(dtm)) {
      count <- dtm[i, j]
      # Skip cells with count == 0 (they remain 0 in both matrices)
      if (count > 0) {
        train_count <- rbinom(1, count, train_prop)
        test_count <- count - train_count
        train_mat[i, j] <- train_count
        test_mat[i, j] <- test_count
      }
      # If count == 0, both train_mat[i, j] and test_mat[i, j] remain 0
    }
  }
  list(train = train_mat, test = test_mat)
}
dtm_split <- split_dtm(dtm, train_prop = 0.8)

dtm_interviste = list(dtm = dtm,
                     dtm_train = dtm_split$train,
                     dtm_test = dtm_split$test)

save(dtm_interviste, file="dtm_interviste.RData")
