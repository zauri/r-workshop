setwd("C:/Users/wenzl/Downloads/R-Schulung/")


deck <- read.csv2("skat_deck.csv")

deal_cards <- function(players, cards_to_deal, deck) {

  player_cards <- vector(mode = "list", length = players)
  
  rest_deck <- deck
  
  for (i in 1:players) {
    
    # choose cards to deal
    cards_to_draw <- sample(nrow(rest_deck), cards_to_deal, replace = FALSE) # replace not possible if cards unique
    
    # deal cards
    player_cards[[i]] <- rest_deck[cards_to_draw ,]
    # remove dealt cards from deck
    rest_deck <- rest_deck[-cards_to_draw, ]   # rest can't include dealt cards
    
    
  }
  
  stopifnot(nrow(dplyr::distinct(dplyr::bind_rows(player_cards))) == players * cards_to_deal)
  # only unique cards, same length as cards_to_deal

}


debug(deal_cards)
deal_cards(5, 5, deck)










