
############################# REVIEWS ###############################

# recensioni: file containing the reviews


# STEP 1: Load required libraries
library(tm)
library(SnowballC)
library(textstem)
library(stringr)
library(hunspell)

# STEP 2: Process the recensioni text (assuming 'review' is column 5)
recensioni$processed_text <- recensioni[,5]

# STEP 3: Convert to lowercase
recensioni$processed_text <- tolower(recensioni$processed_text)

# STEP 4: Remove punctuation, numbers, extra spaces
# The reason substitutions like gsub("\\badvise\\b", "advice", ...) are not being applied as expected
# is because earlier in this block, all punctuation is being replaced with spaces (gsub("[[:punct:]]", " ", ...))
# and sequences of whitespace are collapsed. This means word boundaries ("\\b") as understood by gsub
# are disrupted after removing punctuation, so e.g. ".advise," becomes " advise ", and after trimming spaces, the regex may still work, but edge cases (such as multiple spaces or missing word boundaries due to punctuation removal) can occur.

# To ensure replacements work reliably, apply custom word substitutions *before* removing punctuation,
# numbers, and controlling white space. Here is a correct approach:

# Now proceed with lowercasing, punctuation/number removal, whitespace normalization:

recensioni$processed_text <- gsub("[[:punct:]]", " ", recensioni$processed_text)
recensioni$processed_text <- gsub("[[:digit:]]", " ", recensioni$processed_text)
recensioni$processed_text <- gsub("\\s+", " ", recensioni$processed_text)
recensioni$processed_text <- trimws(recensioni$processed_text)


custom_stops <- c("excellent", "very", "good", "exceptional", "pleasant", "absolutely",
                  "say", "tell", "have", "also", "do", "can", "didn", "in", "fact",
                  "otherwise", "maybe", "many", "like", "look", "make", "may", "main",
                  "really", "whole", "within","without", "just", "let", "know",
                  "absolute", "certainly", "anything", "definitely", "despite",
                  "don", "due", "especially", "everything", "extremely", "first", "put",
                  "get", "go", "give", "however", "lot", "must", "much", "nothing", "one",
                  "rather", "say", "something", "take", "thing", "three", "two", "one", "turn",
                  "use", "us", "unfortunately", "want", "still", "spend", "almost", "already", "although",
                  "always", "come", "even", "every")

# STEP 5: Remove stopwords (custom and standard)
stopwords_en <- stopwords::stopwords("en")
all_stops <- unique(c(stopwords_en, custom_stops))
recensioni$processed_text <- sapply(recensioni$processed_text, function(x) {
  paste(setdiff(unlist(strsplit(x, "\\s+")), all_stops), collapse = " ")
})

# STEP 6: Lemmatize and stem words
recensioni$processed_text <- lemmatize_strings(recensioni$processed_text)
# remove empty lines
recensioni <- subset(recensioni, processed_text != "")

# Instead of lemmatizing the entire string blindly, lemmatize each word but keep only those that have a dictionary meaning.
# We'll split each processed_text row, lemmatize each token, and check if it is a "real" English word (using 'hunspell' or 'wordnet' dictionary).
# If the lemma is a valid word, keep it; otherwise discard the token.
# If you can't load packages here, the following approach works assuming 'hunspell' is installed.

if (requireNamespace("hunspell", quietly = TRUE)) {
  recensioni$processed_text <- sapply(recensioni$processed_text, function(x) {
    tokens <- unlist(strsplit(x, "\\s+"))
    tokens <- tokens[tokens != ""] # remove empty tokens
    lemmas <- lemmatize_words(tokens)
    # Only keep tokens that are real words (in English dictionary)
    is_real_word <- hunspell::hunspell_check(lemmas, dict = hunspell::dictionary("en_US"))
    out <- lemmas[is_real_word]
    if (length(out) == 0) "" else paste(out, collapse = " ")
  })
} else {
  recensioni$processed_text <- sapply(recensioni$processed_text, function(x) {
    tokens <- unlist(strsplit(x, "\\s+"))
    tokens <- tokens[tokens != ""]
    lemmas <- lemmatize_words(tokens)
    # If hunspell not available, fallback: keep all lemmas (risk of non-words)
    if (length(lemmas) == 0) "" else paste(lemmas, collapse = " ")
  })
}
# remove numbers
recensioni$processed_text <- gsub("[[:digit:]]", " ", recensioni$processed_text)
# remove recensioni$processed_text that are not written in english
recensioni$processed_text <- sapply(recensioni$processed_text, function(x) {
  if(is.na(x) || x == "" || trimws(x) == "") {
    return("")
  }
  return(x)
})

# lemmatization and remove non-english texts
recensioni$processed_text <- sapply(recensioni$processed_text, function(x) {
  tokens <- unlist(strsplit(tolower(x), "\\s+"))
  tokens <- tokens[tokens != ""] # Remove empty tokens
  lemmas <- lemmatize_words(tokens)
  # Keep only English words
  is_english <- hunspell_check(lemmas, dict = dictionary("en_US"))
  good_lemmas <- lemmas[is_english]
  if (length(good_lemmas) == 0) return("") else paste(good_lemmas, collapse = " ")
})

# Remove stopwords AGAIN after lemmatization (because lemmatization can transform words into stopwords)
# For example: "excellently" -> "excellent" (which is a stopword)
recensioni$processed_text <- sapply(recensioni$processed_text, function(x) {
  tokens <- unlist(strsplit(x, "\\s+"))
  tokens <- tokens[tokens != ""] # Remove empty tokens
  # Remove stopwords
  tokens <- tokens[!tokens %in% all_stops]
  if (length(tokens) == 0) return("") else paste(tokens, collapse = " ")
})

# First, perform the custom synonym substitutions:
substitutions <- list(
  "\\badvise\\b" = "advice",
  "\\badd\\b" = "addition",
   "\\baccommodate\\b" = "accommodation",
  "\\baccomodate\\b" = "accommodation",
  "\\bbase\\b" = "basic",
  "\\bcleanliness\\b" = "clean",
  "\\bfurnish\\b" = "furniture",
  "\\bkind\\b" = "kindness",
  "\\blove\\b" = "lovely",
  "\\bmanager\\b" = "manage",
  "\\bnewly\\b" = "new",
  "\\blocate\\b" = "location",
  "\\bpossible\\b" = "possibility",
  "\\brelaxation\\b" = "relax",
  "\\bstopover\\b" = "stop",
  "\\bval\\b" = "valley",
  "\\barrival\\b" = "arrive",
  "\\baccessible\\b" = "access",
  "\\battentive\\b" = "attention",
  "\\bavailability\\b" = "available",
  "\\bbeautifully\\b" = "beautiful",
  "\\bbed\\b" = "bedroom",
  "\\bcenter\\b" = "central",
  "\\bchoose\\b" = "choice",
  "\\bcomfortable\\b" = "comfort",
  "\\bcourteous\\b" = "courtesy",
  "\\beasily\\b" = "easy",
  "\\bexpect\\b" = "expectation",
  "\\bfort\\b" = "fortress",
  "\\bfriendliness\\b" = "friendly",
  "\\bhelpfulness\\b" = "helpful",
  "\\bhistoric\\b" = "history",
  "\\bhospitable\\b" = "hospitality",
  "\\bhostess\\b" = "host",
  "\\bminute\\b" = "mini",
  "\\bnearby\\b" = "near",
  "\\bnoise\\b" = "noisy",
  "\\bpeace\\b" = "peaceful",
  "\\bperfectly\\b" = "perfect",
  "\\bprivate\\b" = "privacy",
  "\\brenovate\\b" = "renovation",
  "\\bserve\\b" = "service",
  "\\btaste\\b" = "tasty",
  "\\bvarious\\b" = "variety",
  "\\bvary\\b" = "variety",
  "\\bdecor\\b" = "decoration",
  "\\bdecorate\\b" = "decoration"
)

for (pattern in names(substitutions)) {
  recensioni$processed_text <- gsub(pattern, substitutions[[pattern]], recensioni$processed_text, ignore.case = FALSE)
}

# Remove recensioni that have empty processed_text (i.e., not English or no usable words)
recensioni <- subset(recensioni, processed_text != "" & !is.na(processed_text))


# Create corpus from aggregated text
corpus_recensioni <- VCorpus(VectorSource(recensioni$processed_text))

# Create Document-Term Matrix
dtm_recensioni <- DocumentTermMatrix(corpus_recensioni,
                                     control = list(
                                       tolower = FALSE,  # Already lowercase
                                       removePunctuation = FALSE,  # Already removed
                                       removeNumbers = FALSE,  # Already removed
                                       stopwords = FALSE,  # Already removed
                                       stemming = FALSE,  # Already stemmed
                                       wordLengths = c(2, Inf)  # Minimum word length
                                     ))

rownames(dtm_recensioni) <- recensioni$ID

# Remove terms that start with apostrophe or other problematic characters
cat("Removing terms starting with apostrophes or invalid characters...\n")
terms_to_remove <- grep("^'", colnames(dtm_recensioni), value = TRUE)
if(length(terms_to_remove) > 0) {
  cat("Removing", length(terms_to_remove), "terms starting with apostrophe\n")
  dtm_recensioni <- dtm_recensioni[, !colnames(dtm_recensioni) %in% terms_to_remove]
}

# Also remove terms that don't start with a letter (backup cleanup)
invalid_terms <- grep("^[^a-z]", colnames(dtm_recensioni), value = TRUE)
if(length(invalid_terms) > 0) {
  cat("Removing", length(invalid_terms), "terms with invalid starting characters\n")
  dtm_recensioni <- dtm_recensioni[, !colnames(dtm_recensioni) %in% invalid_terms]
}

dtm <- as.matrix(dtm_recensioni)
dtm <- dtm[,colSums(dtm)>10]
# remove empty rows and columns:
dtm <- dtm[rowSums(dtm) > 0, ]
dtm <- dtm[,colSums(dtm) > 0]

colnames(dtm)
V <- ncol(dtm)
D <- nrow(dtm)

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

dtm_recensioni = list(dtm = dtm,
                      dtm_train = dtm_split$train,
                      dtm_test = dtm_split$test)

save(dtm_recensioni, file="dtm_recensioni.RData")
