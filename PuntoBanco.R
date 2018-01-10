### CASINO SIMULATIONS
### PAUL VAN DER LAKEN
### 2018-01-09
### PAULVANDERLAKEN.COM

# load in packages
pkg <- c("here", "tidyverse", "extrafont")
sapply(pkg, function(x){
  if(!x %in% installed.packages()) install.packages(x)
  library(x, character.only = TRUE)
  return(x)
})
# load in fonts
loadfonts(device = "win")
font <- "Lucida Sans Unicode"

# deck specifications
suits <- c("hearts", "diamonds", "clubs", "spades")
ranks <- c(as.character(2:10), "J", "Q", "K", "A")
values <- c(seq(2,9), 10, 10, 10, 10, 1)
ranks_values <- setNames(values, ranks)
deck_size <- length(ranks) * length(suits)
number_of_decks <- 6
  
# specify simulation settings
simulations <- 50000
hands_per_simulation <- 100
bet_amount <- 100
egalite_payout_mult <- 9

# outcomes
outcome_punto <- "Punto"
outcome_banco <- "Banco"
outcome_banco5 <- "Banco5"
outcome_egalite <- "Egalité"
egalite_label <- paste0(outcome_egalite, " (", egalite_payout_mult, "-1)")


# create empty matrix
col_names <- c("Simulation", "Hand", "Result")
results <- matrix(rep(NA, simulations * hands_per_simulation * length(col_names)), 
                  ncol = length(col_names),
                  dimnames = list(NULL, col_names))

# create single card deck
simulate_deck = function(decks = number_of_decks, shuffle = TRUE, seed = NULL){
  # create empty character string
  cards = character(deck_size * decks)
  # loop through number of decks
  for(d in seq_len(decks)){
    # specify start and end of current deck
    start_deck = 1 + (deck_size * (d - 1))
    end_deck = deck_size + (deck_size * (d - 1))
    # create cards
    cards[start_deck:end_deck] = map(suits, ~ paste0(substring(.x, 1, 1) %>% 
                                                         toupper(), 
                                                         ranks)) %>% 
      unlist()
  }
  # shuffle if specified
  if(shuffle){
    if(!is.null(seed)) set.seed(seed)
    cards <- cards[sample(length(cards))]
  }
  
  return(cards)
  
}

# valuate the cards in hand
valuate = function(cards){
  # transform cards to numeric values
  v <- ranks_values[match(substring(cards, 2), names(ranks_values))]
  # remove tenfolds
  return(sum(v) %% 10)
}

# print the value of the cards in hand
print_hand_value <- function(name, hand){
  print(paste(paste0(name, ":"), 
              paste(hand, collapse = ", "), 
              paste0("(", valuate(hand), ")")))
}


# draw cards from deck to hand
# and remove cards from deck
deal_cards <- function(deck, hand, n_cards = 1){
  # check if there are enough cards 
  if(n_cards < 1) stop("Must deal more than 1 card")
  if(n_cards > length(deck)) stop("Not enough cards in deck")
  # add drawn cards to hand
  hand <- c(hand, deck[seq(n_cards)])
  # remove drawn card form deck
  assign(as.character(substitute(deck)), deck[-1:-n_cards], parent.frame())
  
  return(hand)
}

# simulate one full game of punto banco
simulate_punto_banco = function(deck, print.results = FALSE){

  # check if seven cards left
  if(length(deck) < 7) stop("Less than 7 cards in deck")

  # initiate hands
  player <- banker <- character(0)
  
  # deal cards 
  player <- deal_cards(deck, player)
  banker <- deal_cards(deck, banker)
  player <- deal_cards(deck, player)
  banker <- deal_cards(deck, banker)
  
  # If neither the Player nor Banker is dealt 
  # a total of 8 or 9 in the first two cards (known as a "natural"), 
  # the tableau is consulted, first for Player's rule, then Banker's.
  if(!(valuate(player) >= 8 | valuate(banker) >= 8)){
    
    # If Player has an initial total of 0-5, he draws a third card. 
    # If Player has an initial total of 6 or 7, he stands.
    if(valuate(player) <= 5) {
      
      player <- deal_cards(deck, player)
      
      # If Player drew a third card, the Banker acts according to the following more complex rules:
      value_third_player_card <- valuate(player[length(player)])
      value_banker_cards <- valuate(banker)
      # If Player drew a 2 or 3, Banker draws with 0-4 and stands with 5-7.
      # If Player drew a 4 or 5, Banker draws with 0-5 and stands with 6-7.
      # If Player drew a 6 or 7, Banker draws with 0-6 and stands with 7.
      # If Player drew an 8, Banker draws with 0-2 and stands with 3-7.
      # If Player drew an ace, 9, 10, or face-card, the Banker draws with 0-3 and stands with 4-7.
      if(value_third_player_card == 2 | value_third_player_card == 3){
        if(value_banker_cards <= 4) banker <- deal_cards(deck, banker)
      } else if(value_third_player_card == 4 | value_third_player_card == 5){
        if(value_banker_cards <= 5) banker <- deal_cards(deck, banker)
      } else if(value_third_player_card == 6 | value_third_player_card == 7){
        if(value_banker_cards <= 6) banker <- deal_cards(deck, banker)
      } else if(value_third_player_card == 8){
        if(value_banker_cards <= 2) banker <- deal_cards(deck, banker)
      } else if(value_third_player_card == 1 | value_third_player_card == 9 | value_third_player_card == 0){
        if(value_banker_cards <= 3) banker <- deal_cards(deck, banker)
      }
      
    } else {
      # If Player stood pat (i.e., has only two cards), 
      # the banker regards only his own hand and acts according to the same rule as Player. 
      # That means Banker draws a third card with hands 0-5 and stands with 6 or 7.
      if(valuate(banker) <= 5) banker <- deal_cards(deck, banker)
    }
  } 
  
  # Check result
  if(valuate(player) == valuate(banker)){
    result = outcome_egalite
  } else if(valuate(player) > valuate(banker)){
    result = outcome_punto
  } else if(valuate(player) < valuate(banker)){
    result = ifelse(valuate(banker) == 5, outcome_banco5, outcome_banco)
  } 
  
  # Print results
  if(print.results){
    print_hand_value("Player", player)
    print_hand_value("Banker", banker)
    print(result)
  }
  
  # Return result list
  results_list = list(deck = deck,
                      result = result,
                      player_hand = player,
                      player_score = valuate(player),
                      banker_hand = banker,
                      banker_score = valuate(banker))
  return(results_list)
}


### RUN ACTUAL SIMULATIONS

# run simulations
for(s in seq(simulations)){
  
  # reset hand
  h <- NULL
  
  # simulate hands
  for(h in seq(hands_per_simulation)){
    
    # specify matrix row number of current hand
    row_nr <- ((s - 1) * hands_per_simulation) + h
    
    # simulate initial deck
    if(h == 1){ d <- simulate_deck(decks = number_of_decks) }
    # simulate new deck if current one finished
    if(length(d) < 7){ d <- simulate_deck(decks = number_of_decks)}
    
    # simulate game
    game_results <- simulate_punto_banco(d)
    
    # replace deck with remainder after simulation
    d <- game_results$deck
    
    # store result in matrix
    results[row_nr, ] <- c(s, h, game_results$result)
    
  }
  # print progress
  if((s %% 100) == 0) print(s)
}

compute_payoff <- function(result, bet){
  # result == NA
  if(is.na(bet)) return(payoff <- NA)
  if(bet == outcome_banco5) bet <- outcome_banco
  # result == egalite
  if(result == outcome_egalite){
    if(bet == outcome_egalite) payoff <- bet_amount * egalite_payout_mult
    else if(bet == outcome_punto) payoff <- 0
    else if(bet == outcome_banco) payoff <- 0
    # result == punto
  } else if(result == outcome_punto){
    if(bet == outcome_egalite) payoff <- -bet_amount
    else if(bet == outcome_punto) payoff <- bet_amount
    else if(bet == outcome_banco) payoff <- -bet_amount
    # result == banco
  } else if(result == outcome_banco | result == outcome_banco5){
    if(bet == outcome_egalite) payoff <- -bet_amount
    else if(bet == outcome_punto) payoff <- -bet_amount
    else if(bet == outcome_banco & result == outcome_banco) payoff <- bet_amount
    else if(bet == outcome_banco & result == outcome_banco5) payoff <- bet_amount / 2
  }
  return(payoff)
}


# Compute Strategy Payoffs
results %>%
  as.tibble() %>%
  mutate(Simulation = as.numeric(Simulation)) %>%
  mutate(Hand = as.numeric(Hand)) %>%
  # compute profit for simple strategies
  mutate(Egalité = map2_dbl(Result, outcome_egalite, ~ compute_payoff(.x, .y))) %>%
  mutate(Punto = map2_dbl(Result, outcome_punto, ~ compute_payoff(.x, .y))) %>%
  mutate(Banco = map2_dbl(Result, outcome_banco, ~ compute_payoff(.x, .y))) %>%
  # compute profit for last hand strategies
  group_by(Simulation) %>%
  mutate(lagResult1 = lag(Result, 1)) %>% # lag last hand
  mutate(LastHand = map2_dbl(Result, lagResult1, ~ compute_payoff(.x, .y))) %>%
  mutate(LastHand_PB = ifelse(lagResult1 == outcome_egalite | is.na(lagResult1), 
                             NA, LastHand)) %>%
  select(-lagResult1) %>% # remove last hand
  # dataframe to long format
  gather(Strategy, Profit, -Simulation, -Hand, -Result) %>%
  # specify order of strategies
  mutate(Strategy = factor(Strategy, 
                           levels = c(outcome_punto, outcome_banco, outcome_egalite,
                                      "LastHand", "LastHand_PB"),
                           labels = c(outcome_punto, outcome_banco, egalite_label,
                                     "LastHand", "LastHand_PB"))) %>%
  # some strategies don't play each hand
  group_by(Simulation, Strategy) %>%
  mutate(HandPlayed = cumsum(!is.na(Profit))) %>%
  # compute cumulative profit
  mutate(Profit = cumsum(ifelse(is.na(Profit), 0, Profit))) %>%
  arrange(Simulation, Strategy, Hand) ->
  results_cum

results_cum %>%
  filter(HandPlayed > 0) %>%
  # filter(Simulation < 10) %>%
  ggplot(aes(x = HandPlayed, y = Profit, col = Strategy)) +
  geom_hline(yintercept = 0, lty = 5, col = "grey70", size = 1) +
  geom_hline(yintercept = -bet_amount, lty = 5, col = "red", size = 1) +
  geom_hline(yintercept = bet_amount, lty = 5, col = "green", size = 1) +
  # geom_line(aes(group = interaction(Simulation, Strategy)), alpha = 0.1) +
  geom_smooth(aes(group = Strategy), method = "gam", size = 1.5, se = FALSE) +
  annotate("label", x = hands_per_simulation/2, y = 0, label = "No profit/loss", 
           label.size = NA, vjust = 0, hjust = 0.5, col = "grey70", fill = "transparent",
           size = 3.5, family = font) +
  annotate("label", x = 5, y = -bet_amount, label = "Lost bet amount", 
           label.size = NA, vjust = 0, hjust = 0.5, col = "red", fill = "transparent",
           size = 3.5, family = font) +
  annotate("label", x = 5, y = bet_amount, label = "Won bet amount", 
           label.size = NA, vjust = 1, hjust = 0.5, col = "green", fill = "transparent",
           size = 3.5, family = font) +
  scale_color_manual(values = c("red", "yellow2", "lightblue", "green3", "blue")) +
  labs(y = "Average Profit (% of bet size)",
       x = "Number of Hands Played",
       title = "The House Always Wins At Baccarat (Punto Banco)",
       subtitle = paste(
         paste("Average payoff of", format(simulations, big.mark = ","), 
               "simulated playthroughs of", hands_per_simulation, "hands of North American baccarat"),
         "Every strategy results in losses. Best to bet only Banco and avoid Egalité completely (even 9-1).",
         sep = "\n"),
       caption = "github.com/paulvanderlaken/2018_casino   //   paulvanderlaken.com") +
  coord_cartesian(ylim = c(-bet_amount*2, bet_amount)) +
  theme_light() +
  theme(legend.position = c(0.5, 1), 
        legend.justification = c(0.5, 1),
        legend.background = element_rect(colour = "grey70", size = rel(0.5)),
        legend.direction = "horizontal",
        text = element_text(family = font),
        plot.title = element_text(size = 20))
ggsave(here(paste0("images/average_payoff_PB_",
                   simulations, "s",
                   hands_per_simulation, "h",
                   ".png")),
       dpi = 600, width = 9, height = 6)
