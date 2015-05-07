###############################
# Read data and get sentiment #
###############################

# Sentiment analysis library
library(syuzhet)

# Project Gutenberg: Best Russian Short Stories
# https://www.gutenberg.org/cache/epub/13437/pg13437.txt
# Folder where the text files are located
folder <- "best_russian_short_stories"

# Empty list where to append the sentiment
sent_list <- list()

# Read each file and save the sentiment in a list
for (i in dir(folder)){
    path <- paste(folder, i, sep="/")
    lines <- get_sentences(readLines(path))
    sent <- get_sentiment(lines, method = "bing")
    sent_list[[i]] <- sent
}

rm(i, path, folder, lines, sent)

# Transform values
transf_list <- list()

for (i in 1:length(sent_list)){
    transf <- get_transformed_values(sent_list[[i]], 
                           low_pass_size = 3, 
                           x_reverse_len = 100,
                           scale_vals = FALSE,
                           scale_range = TRUE
                           )
    transf_list[[i]] <- transf
}

rm(i, transf)

#########################
# Work titles & authors #
#########################
# Save the work title and authors to use them later in plots

# TITLE
title <- sapply(strsplit(names(sent_list),
                         split = "__"),
                "[",1)

# Replace underscores with spaces
title <- gsub("_", " ", title)

# AUTHOR
author <- sapply(strsplit(names(sent_list),
                         split = "__"),
                "[",2)

# Remove ".txt" at the end
author <- gsub(".txt", "", author)

#######################################
# Example: De profundis - Oscar Wilde #
#######################################

lines <- get_sentences(readLines("de_profundis_oscar_wilde.txt"))
sent <- get_sentiment(lines, method = "bing")
transf <- get_transformed_values(sent, low_pass_size = 3, x_reverse_len = 100,
                                 scale_vals = FALSE, scale_range = TRUE)

par(las = 1)
plot(
    transf, type ="h", main = "Oscar Wilde\nDe profundis", 
    xlab = "Narrative Time", 
    ylab = "Emotional Valence", 
    col = "#74A3D6",
    axes = FALSE,
    lwd = 4
)
ticks_x = seq(0, 100, 25)
ticks_y = seq(-1, 1, 1)
axis(1, at = ticks_x, tck = 0.02)
axis(2, at = ticks_y, tck = 0.02)

rm(lines, sent, transf, ticks_x, ticks_y)

#############################
# Small multiples sentiment #
#############################

par(mfrow=c(5,4), mar = c(1,3,3,1), las=1, bty="n")

# Start with an empty plot
plot.new()

for (i in 1:length(transf_list)){
    plot(
        transf_list[[i]], 
        type ="h", 
        main = paste(author[i], title[i], sep = "\n"), 
        xlab = "Narrative Time", 
        ylab = "Emotional Valence", 
        col = "#5A91CC",
        axes = FALSE
    )
#     axis(1, tick=FALSE, col.axis="white")
#     axis(2, tick=FALSE, col.axis="white")
}

rm(i)

###############
# Get emotion #
###############

folder <- "best_russian_short_stories"

# Empty data frame where to append the emotions
emot_df <- data.frame()

# Read each file and save the emotiment in a list
for (i in dir(folder)){
    path <- paste(folder, i, sep="/")
    lines <- get_sentences(readLines(path))
    emot <- colSums(get_nrc_sentiment(lines))
    emot_df <- rbind(emot_df, emot)
}

names(emot_df) <- names(emot)
rm(i, path, folder, lines, emot)


###########################
# Small multiples emotion #
###########################

par(mfrow=c(5,4), mar = c(1,3,4,1), las=1, bty="n")

# Start with an empty plot
plot.new()

for (i in 1:nrow(emot_df)){
    barplot(as.numeric(emot_df[i,c(9,10)]),
            main = paste(author[i], title[i], sep = "\n"), 
            names.arg=c("Neg", "Pos"),
            col = c("#D47A73", "#5A91CC"),
            axes = FALSE)
}

rm(i)


####################
# Bar plot emotion #
####################
par(mfrow=c(1,1), mar = c(3,6,3,3), las=1, bty="n")

barplot(sort(colSums(prop.table(emot_df[1:8]))),
        horiz = TRUE, axes = F, col = c("lightgrey", "lightgrey", "lightgrey", 
                                        "lightgrey", "#8C9934", "#2B547F", "lightgrey", "lightgrey"))


# Small multiples
par(mfrow=c(5,4), mar = c(1,3,4,1), las=1, bty="n")

# Start with an empty plot
plot.new()

for (i in 1:nrow(emot_df)){
    barplot(colSums(emot_df[i, 6:5]),
            axes = F, col = c("#2B547F", "#8C9934"),
            main = paste(author[i], title[i], sep = "\n"))
}
