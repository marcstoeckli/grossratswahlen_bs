
#### Create Data Set ####

data_final_2024_all <- data_2024 %>% 
  select(Wahlkreisbezeichnung, `Anzahl Sitze`, 
         `Listen-Nr`, Parteikurzbezeichnung, Wahlzettel,
         `Anzahl Sitze Liste`,
         `Kandidatenstimmen unveränderte Wahlzettel`,
         `Kandidatenstimmen veränderte Wahlzettel`,
         `00 OHNE`, `01 FDP`, `03 LDP`, `04 EVP`, `05 SP`, 
         `07 Mitte`, `09 EDU`, `10 GLP`, `11 PdA`, `12 SVP`,
         `14 VA`, `16 AB`, `28 PBkW`, `43 GRÜNE`, `45 BastA`,
         `46 FSSK`, `47 KUSS`,
         `Kandidaten-Nr`, `Personen-ID`,
         Name, Vorname, Geschlecht,
         `Alter`, Bisher, Gewählt,
         `Stimmen unveränderte Wahlzettel`, `Stimmen veränderte Wahlzettel`,
         `Stimmen Total aus Wahlzettel`, Rangfolge) %>% 
  rename(list = `Listen-Nr`,
         party = Parteikurzbezeichnung,
         seats = `Anzahl Sitze Liste`,
         kreis = Wahlkreisbezeichnung,
         votes_kreis_party_unchanged = `Kandidatenstimmen unveränderte Wahlzettel`,
         votes_kreis_party_changed = `Kandidatenstimmen veränderte Wahlzettel`,
         votes_by_ohne = `00 OHNE`, 
         votes_by_FDP = `01 FDP`, 
         votes_by_LDP = `03 LDP`, 
         votes_by_EVP = `04 EVP`, 
         votes_by_SP = `05 SP`, 
         votes_by_Mitte = `07 Mitte`, 
         votes_by_EDU = `09 EDU`,
         votes_by_GLP = `10 GLP`, 
         votes_by_PdA =`11 PdA`,
         votes_by_SVP = `12 SVP`, 
         votes_by_VA = `14 VA`,
         votes_by_AB = `16 AB`,
         votes_by_PBkW = `28 PBkW`,
         votes_by_Gruene = `43 GRÜNE`, 
         votes_by_BastA = `45 BastA`,
         votes_by_FSSK = `46 FSSK`,
         votes_by_KUSS = `47 KUSS`,
         name = Name,
         firstname = Vorname,
         age = `Alter`,
         gender = Geschlecht,
         ballot = Wahlzettel,
         candidate_id = `Kandidaten-Nr`,
         incumbent = Bisher,
         elected = `Gewählt`,
         votes_unchanged = `Stimmen unveränderte Wahlzettel`,
         votes_changed = `Stimmen veränderte Wahlzettel`,
         votes = `Stimmen Total aus Wahlzettel`,
         rank = Rangfolge) %>% 
  mutate(kreis = str_remove(kreis, "Wahlkreis "),
         list_place = as.numeric(substr(candidate_id, 3, 4)),
         votes_kreis_party = as.numeric(votes_kreis_party_changed) + as.numeric(votes_kreis_party_unchanged),
         share = votes / votes_kreis_party,
         share_changed = votes_changed / votes_kreis_party,
         incumbent = case_when(incumbent == "bisher" ~ TRUE,
                               is.na(incumbent) ~ FALSE),
         elected = case_when(elected == "Gewählt" ~ TRUE,
                             elected == "nicht gewählt" ~ FALSE),
         votes_by_own = case_when(party == "FDP" ~ votes_by_FDP,
                                  party == "LDP" ~ votes_by_LDP,
                                  party == "EVP" ~ votes_by_EVP,
                                  party == "SP" ~ votes_by_SP,
                                  party == "Mitte" ~ votes_by_Mitte,
                                  party == "EDU" ~ votes_by_EDU,
                                  party == "GLP" ~ votes_by_GLP,
                                  party == "PdA" ~ votes_by_PdA,
                                  party == "SVP" ~ votes_by_SVP,
                                  party == "VA" ~ votes_by_VA,
                                  party == "AB" ~ votes_by_AB,
                                  party == "PBkW" ~ votes_by_PBkW,
                                  party == "GRÜNE" ~ votes_by_Gruene,
                                  party == "BastA" ~ votes_by_BastA,
                                  party == "FSSK" ~ votes_by_FSSK,
                                  party == "KUSS" ~ votes_by_KUSS)) %>% 
  rowwise() %>% 
  mutate(votes_by_others = sum(across(starts_with("votes_by")), na.rm = TRUE) - 2*votes_by_own) %>%
  group_by(kreis, party) %>% 
  mutate(share_others = votes_by_others/votes_kreis_party,
         share_own = votes_by_own/votes_kreis_party,
         share_scale = as.numeric(scale(share, center = TRUE, scale = TRUE)),
         share_changed_scale = as.numeric(scale(share_changed, center = TRUE, scale = TRUE)),
         share_others_scale = as.numeric(scale(share_others, center = TRUE, scale = TRUE)),
         share_own_scale = as.numeric(scale(share_own, center = TRUE, scale = TRUE)),
         rank_own = as.integer(rank(-share_own)),
         rank_others = as.integer(rank(-share_others))) %>% 
  ungroup() %>%
  filter(!(party %in% c("AB", "BDV", "PB", "PP", "EDU",
                        "fuk", "KL", "VA", "EVP", "PdA",
                        "FSSK", "KUSS", "PBkW")),
         kreis != "Bettingen") %>% 
  group_by(kreis, list) %>%
  mutate(list_total = max(list_place)) %>% 
  ungroup() %>% 
  group_by(kreis, list, gender) %>%
  mutate(list_total_gender = max(list_place)) %>% 
  ungroup() %>% 
  mutate(place_last = list_place == list_total,
         place_last_gender = list_place == list_total_gender,
         rank_nachrueck = rank - seats) %>% 
  arrange(kreis, party, list_place) %>% 
  group_by(party, kreis) %>% 
  mutate(below = (lag(incumbent) == TRUE) &
           (incumbent == FALSE),
         below_real = lag(incumbent) == TRUE,
         above = (lead(incumbent) == TRUE) &
           (incumbent == FALSE),
         above_real = lead(incumbent) == TRUE,
         last_four = row_number() > (n() - 4),
         first_four = row_number() <= 4) %>% 
  group_by(party, kreis, gender) %>% 
  mutate(last_two_gender = row_number() > (n() - 2),
         first_two_gender = row_number() <= 2,
         last_three_gender = row_number() > (n() - 3),
         first_three_gender = row_number() <= 3) %>% 
  ungroup() %>%
  mutate(below = replace(below, is.na(below), FALSE),
         below_real = replace(below_real, is.na(below_real), FALSE),
         above = replace(above, is.na(above), FALSE),
         above_real = replace(above_real, is.na(above_real), FALSE),
         either = below | above,
         either_real = below_real | above_real,
         year = "2024")

data_final_2024 <- data_final_2024_all %>% 
  filter(party == "SP")

data_final_2020_all <- data_2020 %>% 
  select(Wahlkreisbezeichnung, `Anzahl Sitze Wahlkreis`, 
         Listennummer, Parteikurzbezeichnung, Wahlzettel,
         `Anzahl Sitze Liste`,
         `Kandidatenstimmen unveränderte Wahlzettel`,
         `Kandidatenstimmen veränderte Wahlzettel`,
         `00 Ohne`, `01 FDP`, `02 PP`, `03 LDP`, `04 EVP`, `05 SP`, 
         `07 CVP`, `08 GB`, `10 GLP`, `12 SVP`, `13 FUK`, `14 VA`, 
         #`16 AB`, `19 BDV`, 
         `28 PB`, `29 KL`,
         `Kandidaten-Nr`, `Personen-ID`,
         Name, Vorname, Geschlecht,
         `Alter am Jahresende 2020`, Bisher, Gewählt,
         `Stimmen unveränderte Wahlzettel`, `Stimmen veränderte Wahlzettel`,
         `Stimmen Total`, Rangfolge) %>% 
  rename(list = Listennummer,
         party = Parteikurzbezeichnung,
         seats = `Anzahl Sitze Liste`,
         kreis = Wahlkreisbezeichnung,
         votes_kreis_party_unchanged = `Kandidatenstimmen unveränderte Wahlzettel`,
         votes_kreis_party_changed = `Kandidatenstimmen veränderte Wahlzettel`,
         votes_by_ohne = `00 Ohne`, 
         votes_by_FDP = `01 FDP`, 
         votes_by_PP = `02 PP`, 
         votes_by_LDP = `03 LDP`, 
         votes_by_EVP = `04 EVP`, 
         votes_by_SP = `05 SP`, 
         votes_by_CVP = `07 CVP`, 
         votes_by_GB = `08 GB`, 
         votes_by_GLP = `10 GLP`, 
         votes_by_SVP = `12 SVP`, 
         votes_by_FUK = `13 FUK`, 
         votes_by_VA = `14 VA`, 
         #votes_by_AB = `16 AB`, 
         #votes_by_BDV = `19 BDV`,
         votes_by_PB = `28 PB`, 
         votes_by_KL = `29 KL`,
         name = Name,
         firstname = Vorname,
         age = `Alter am Jahresende 2020`,
         gender = Geschlecht,
         ballot = Wahlzettel,
         candidate_id = `Kandidaten-Nr`,
         incumbent = Bisher,
         elected = `Gewählt`,
         votes_unchanged = `Stimmen unveränderte Wahlzettel`,
         votes_changed = `Stimmen veränderte Wahlzettel`,
         votes = `Stimmen Total`,
         rank = Rangfolge) %>%
  mutate(list_place = as.numeric(substr(candidate_id, 3, 4)),
         votes_kreis_party = as.numeric(votes_kreis_party_changed) + as.numeric(votes_kreis_party_unchanged),
         share = votes / votes_kreis_party,
         share_changed = votes_changed / votes_kreis_party,
         incumbent = case_when(incumbent == "bisher" ~ TRUE,
                               incumbent == "nicht amtierend" ~ FALSE),
         elected = case_when(elected == "Gewählt" ~ TRUE,
                             elected == "nicht gewählt" ~ FALSE),
         votes_by_own = case_when(party == "FDP" ~ votes_by_FDP,
                                  party == "PP" ~ votes_by_PP,
                                  party == "LDP" ~ votes_by_LDP,
                                  party == "EVP" ~ votes_by_EVP,
                                  party == "SP" ~ votes_by_SP,
                                  party == "CVP" ~ votes_by_CVP,
                                  party == "GB" ~ votes_by_GB,
                                  party == "GLP" ~ votes_by_GLP,
                                  party == "SVP" ~ votes_by_SVP,
                                  party == "FUK" ~ votes_by_FUK,
                                  party == "VA" ~ votes_by_VA,
                                  #party == "AB" ~ votes_by_AB,
                                  #party == "BDV" ~ votes_by_BDV,
                                  party == "PB" ~ votes_by_PB,
                                  party == "KL" ~ votes_by_KL)) %>% 
  rowwise() %>% 
  mutate(votes_by_others = sum(across(starts_with("votes_by")), na.rm = TRUE) - 2*votes_by_own) %>% 
  group_by(kreis, party) %>% 
  mutate(share_others = votes_by_others/votes_kreis_party,
         share_own = votes_by_own/votes_kreis_party,
         share_scale = as.numeric(scale(share, center = TRUE, scale = TRUE)),
         share_changed_scale = as.numeric(scale(share_changed, center = TRUE, scale = TRUE)),
         share_others_scale = as.numeric(scale(share_others, center = TRUE, scale = TRUE)),
         share_own_scale = as.numeric(scale(share_own, center = TRUE, scale = TRUE)),
         rank_own = as.integer(rank(-share_own)),
         rank_others = as.integer(rank(-share_others))) %>% 
  ungroup() %>%
  filter(!(party %in% c("AB", "BDV", "PB", "PP", 
                        "fuk", "KL", "VA", "EVP"))) %>% 
  group_by(kreis, list) %>%
  mutate(list_total = max(list_place)) %>% 
  ungroup() %>% 
  group_by(kreis, list, gender) %>%
  mutate(list_total_gender = max(list_place)) %>% 
  ungroup() %>% 
  mutate(place_last = list_place == list_total,
         place_last_gender = list_place == list_total_gender,
         rank_nachrueck = rank - seats) %>% 
  arrange(kreis, party, list_place) %>% 
  group_by(party, kreis) %>% 
  mutate(below = (lag(incumbent) == TRUE) &
           (incumbent == FALSE),
         below_real = lag(incumbent) == TRUE,
         above = (lead(incumbent) == TRUE) &
           (incumbent == FALSE),
         above_real = lead(incumbent) == TRUE,
         last_four = row_number() > (n() - 4),
         first_four = row_number() <= 4) %>% 
  group_by(party, kreis, gender) %>% 
  mutate(last_two_gender = row_number() > (n() - 2),
         first_two_gender = row_number() <= 2,
         last_three_gender = row_number() > (n() - 3),
         first_three_gender = row_number() <= 3) %>% 
  ungroup() %>% 
  mutate(below = replace(below, is.na(below), FALSE),
         below_real = replace(below_real, is.na(below_real), FALSE),
         above = replace(above, is.na(above), FALSE),
         above_real = replace(above_real, is.na(above_real), FALSE),
         either = below | above,
         either_real = below_real | above_real,
         party = replace(party, party == "CVP", "Mitte"),
         year = "2020")

data_final_2020 <- data_final_2020_all %>% 
  filter(party == "SP")

data_final_2016 <- data_2016 %>% 
  mutate(list_place = as.integer(list_place),
         incumbent = case_when(incumbent == "Ja" ~ TRUE,
                               incumbent == "Nein" ~ FALSE),
         elected = case_when(elected == "Ja" ~ TRUE,
                             elected == "Nein" ~ FALSE)) %>% 
  group_by(kreis) %>% 
  mutate(votes_kreis_party = sum(votes),
         list_total = max(list_place)) %>% 
  ungroup() %>% 
  mutate(share = votes/votes_kreis_party) %>% 
  arrange(kreis, list_place) %>% 
  group_by(kreis) %>% 
  mutate(share_scale = as.numeric(scale(share, center = TRUE, scale = TRUE)),
         below = (lag(incumbent) == TRUE) &
           (incumbent == FALSE),
         above = (lead(incumbent) == TRUE) &
           (incumbent == FALSE),
         either = below | above,
         last_four = row_number() > (n() - 4),
         first_four = row_number() <= 4) %>%
  group_by(kreis, gender) %>% 
  mutate(last_two_gender = row_number() > (n() - 2),
         first_two_gender = row_number() <= 2,
         last_three_gender = row_number() > (n() - 3),
         first_three_gender = row_number() <= 3) %>%  
  ungroup() %>% 
  arrange(kreis, desc(share_scale)) %>% 
  group_by(kreis) %>% 
  mutate(rank = as.integer(rank(-share_scale))) %>% 
  ungroup() %>% 
  mutate(place_last = list_place == list_total) %>% 
  mutate(below = replace(below, is.na(below), FALSE),
         above = replace(above, is.na(above), FALSE),
         either = below | above,
         year = "2016")

data_final_2012 <- data_2012 %>% 
  mutate(list_place = as.integer(list_place),
         incumbent = case_when(incumbent == "Ja" ~ TRUE,
                               incumbent == "Nein" ~ FALSE),
         elected = case_when(elected == "Ja" ~ TRUE,
                             elected == "Nein" ~ FALSE)) %>%
  group_by(kreis) %>% 
  mutate(votes_kreis_party = sum(votes),
         list_total = max(list_place)) %>%
  ungroup() %>% 
  mutate(share = votes/votes_kreis_party) %>% 
  arrange(kreis, list_place) %>%
  group_by(kreis) %>% 
  mutate(share_scale = as.numeric(scale(share, center = TRUE, scale = TRUE)),
         below = (lag(incumbent) == TRUE) &
           (incumbent == FALSE),
         above = (lead(incumbent) == TRUE) &
           (incumbent == FALSE),
         either = below | above,
         last_four = row_number() > (n() - 4),
         first_four = row_number() <= 4) %>%
  group_by(kreis, gender) %>% 
  mutate(last_two_gender = row_number() > (n() - 2),
         first_two_gender = row_number() <= 2,
         last_three_gender = row_number() > (n() - 3),
         first_three_gender = row_number() <= 3) %>% 
  ungroup() %>% 
  arrange(kreis, desc(share_scale)) %>% 
  group_by(kreis) %>% 
  mutate(rank = as.integer(rank(-share_scale))) %>% 
  ungroup() %>% 
  mutate(place_last = list_place == list_total) %>% 
  mutate(year = "2012",
         below = replace(below, is.na(below), FALSE),
         above = replace(above, is.na(above), FALSE),
         either = below | above)

data_final_2008 <- data_2008 %>% 
  mutate(list_place = as.integer(list_place),
         incumbent = case_when(incumbent == "Ja" ~ TRUE,
                               incumbent == "Nein" ~ FALSE),
         elected = case_when(elected == "Ja" ~ TRUE,
                             elected == "Nein" ~ FALSE)) %>% 
  group_by(kreis) %>% 
  mutate(votes_kreis_party = sum(votes),
         list_total = max(list_place)) %>% 
  ungroup() %>% 
  mutate(share = votes/votes_kreis_party) %>% 
  arrange(kreis, list_place) %>% 
  group_by(kreis) %>% 
  mutate(share_scale = as.numeric(scale(share, center = TRUE, scale = TRUE)),
         below = (lag(incumbent) == TRUE) &
           (incumbent == FALSE),
         above = (lead(incumbent) == TRUE) &
           (incumbent == FALSE),
         either = below | above,
         last_four = row_number() > (n() - 4),
         first_four = row_number() <= 4) %>%
  group_by(kreis, gender) %>% 
  mutate(last_two_gender = row_number() > (n() - 2),
         first_two_gender = row_number() <= 2,
         last_three_gender = row_number() > (n() - 3),
         first_three_gender = row_number() <= 3) %>% 
  ungroup() %>% 
  arrange(kreis, desc(share_scale)) %>% 
  group_by(kreis) %>% 
  mutate(rank = as.integer(rank(-share_scale))) %>% 
  ungroup() %>% 
  mutate(place_last = list_place == list_total) %>% 
  mutate(below = replace(below, is.na(below), FALSE),
         above = replace(above, is.na(above), FALSE),
         either = below | above,
         year = "2008")

data_final <- rbind(data_final_2024 %>% 
                      mutate(name = paste(name, firstname)) %>% 
                      select(kreis, list_place, name, gender, incumbent, votes,
                             elected, votes_kreis_party, list_total, share,
                             share_scale, below, above, either, last_four, first_four,
                             last_two_gender, first_two_gender, last_three_gender, first_three_gender,
                             rank, place_last, year),
                    data_final_2020 %>% 
                    mutate(name = paste(name, firstname)) %>% 
                    select(kreis, list_place, name, gender, incumbent, votes,
                           elected, votes_kreis_party, list_total, share,
                           share_scale, below, above, either, last_four, first_four,
                           last_two_gender, first_two_gender, last_three_gender, first_three_gender,
                           rank, place_last, year),
                  data_final_2016 %>% 
                    select(kreis, list_place, name, gender, incumbent, votes,
                           elected, votes_kreis_party, list_total, share,
                           share_scale, below, above, either, last_four, first_four,
                           last_two_gender, first_two_gender, last_three_gender, first_three_gender,
                           rank, place_last, year),
                  data_final_2012 %>% 
                    select(kreis, list_place, name, gender, incumbent, votes,
                           elected, votes_kreis_party, list_total, share,
                           share_scale, below, above, either, last_four, first_four,
                           last_two_gender, first_two_gender, last_three_gender, first_three_gender,
                           rank, place_last, year),
                  data_final_2008 %>% 
                    select(kreis, list_place, name, gender, incumbent, votes,
                           elected, votes_kreis_party, list_total, share,
                           share_scale, below, above, either, last_four, first_four,
                           last_two_gender, first_two_gender, last_three_gender, first_three_gender,
                           rank, place_last, year)) %>% 
  arrange(kreis, year, list_place) %>% 
  group_by(kreis, year, gender) %>% 
  mutate(rank_gender = ifelse(gender == "M", rank(list_place), 
                              rank(desc(list_place)))) %>% 
  ungroup()

data_final_all <- rbind(data_final_2024_all %>% 
                          select(kreis, party, list_place, name, gender, age, incumbent, votes,
                                 elected, votes_kreis_party, list_total, share,
                                 share_scale, below, above, either, last_four, first_four,
                                 last_two_gender, first_two_gender, last_three_gender, first_three_gender,
                                 rank, rank_own, rank_others, place_last, year),
                        data_final_2020_all %>% 
                          select(kreis, party, list_place, name, gender, age, incumbent, votes,
                                 elected, votes_kreis_party, list_total, share,
                                 share_scale, below, above, either, last_four, first_four,
                                 last_two_gender, first_two_gender, last_three_gender, first_three_gender,
                                 rank, rank_own, rank_others, place_last, year))

save(data_final, file = "./data/processed/data_final.Rdata")
save(data_final_all, file = "./data/processed/data_final_all.Rdata")
