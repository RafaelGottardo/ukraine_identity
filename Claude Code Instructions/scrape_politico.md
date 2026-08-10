

From Politico's poll of polls (e.g., https://www.politico.eu/europe-poll-of-polls/bulgaria/) write and run scrpaing code in a file called `scripts/scrape_polls.py` For the following countries:

 "Croatia"        "Denmark"        "Finland"        "France"         "Germany"       
 "Greece"         "Hungary"        "Italy"          "Lithuania"      "Netherlands"   
 "Poland"         "Romania"        "Slovakia"       "Spain"          "Sweden"        
 "UK"             "Austria"        "Belgium"        "Bulgaria"       "Czech Republic"
"Ireland"        "Portugal"  

Scrape how each party was polling in April 2022, April 2023, April 2024, April 2025, and April 2026.  The scraping code should output a .csv in the data_clean folder with the party's name in English and native language a column for each poll result and election result percentage. Use the tables on pages 3-17 of `data_raw/1999-2024_CHES_codebook.pdf` to pull match parties to their `eu_russia` from `1999-2024_CHES_dataset_meansV2.csv` if a party is missing put try to pull `eu_russia` from `CHES_Ukraine_March_2024.dta` otherwise put "NA" for `eu_russia`, do not guess for `eu_russia`. If availbe you can also match `new_q59` from `data_raw/Copy of script 29.04.2025.docx`. Paste a list of any parties with NA for eu_russia below:

## Parties with "NA" for eu_russia

123 of 305 scraped parties (`data_clean/politico_polls.csv`) could not be matched to a `eu_russia` value in either `1999-2024_CHES_dataset_meansV2.csv` or the `CHES_Ukraine_March_2024.dta` fallback. Parties are matched to a CHES party ID via the abbreviation tables on pages 3-17 of `data_raw/1999-2024_CHES_codebook.pdf` (exact/prefix match on POLITICO's polling code or native name, falling back to name-overlap against the tables' own party names); most of the parties below simply have no counterpart in those tables (newer splinter parties, or the sub-annual "(-YYYY)" POLITICO relabelings of an already-listed party). Multi-party electoral coalitions (POLITICO codes tagged "coal") are deliberately left unmatched rather than attributed to one member party's score.

- Austria: BIER
- Austria: Communist Party of Austria (Kommunistische Partei Österreichs)
- Austria: MFG
- Belgium: Anders
- Belgium: DéFI
- Bulgaria: Attack
- Bulgaria: BV (–2024)
- Bulgaria: IMRO (–2023)
- Bulgaria: IS.BG
- Bulgaria: PP+DB
- Bulgaria: Progressive Bulgaria (Прогресивна България)
- Bulgaria: The Left! (–2024)
- Croatia: DPMŠ
- Croatia: KH (–2024)
- Croatia: We Can! – Political Platform (Možemo! - politička platforma)
- Czech Republic: CESKO
- Czech Republic: Czech Pirate Party (Česká pirátská strana)
- Czech Republic: PRO
- Czech Republic: PRO (–2025)
- Czech Republic: Piráti+STAN (-2023)
- Czech Republic: SPD (SPD + THO + Svobodní + PRO) (–2026)
- Czech Republic: SPOLU (2020-2023)
- Czech Republic: Spolu (ODS + TOP09 + KDU-ČSL) (–2026)
- Czech Republic: Stačilo!
- Czech Republic: Svobodní (–2025)
- Czech Republic: T+S (-2022)
- Czech Republic: THO (–2025)
- Czech Republic: ZELENI
- Czech Republic: Zelení (—2025)
- Denmark: A (Å)
- Denmark: BP
- Denmark: G
- Denmark: KD (K)
- Denmark: M
- Denmark: NB (D)(–2024)
- Denmark: RG (Ø)
- Denmark: RV (B)
- Finland: LIIK
- Finland: SFP
- France: LO/NPA (–2024)
- France: Miscellaneous Left (Divers gauche)
- France: Miscellaneous Right (Diverse droite)
- France: New Popular Front (Nouveau Front populaire)
- France: Others (left)
- France: Others (right)
- France: PS (-2022)
- France: The Republicans/Union of the Right and Centre (Les Républicains/Union de la droite et du centre)
- Greece: DPK
- Greece: EK
- Greece: Elas - Hellenic Left Alignment (Ελληνική Αριστερή Συμπαράταξη)
- Greece: Hope for Democracy (Ελπίδα για την Δημοκρατία)
- Greece: MeRA25
- Greece: Movement for Change (Kinima Allagis)
- Greece: NEA
- Greece: NIKH
- Hungary: DK (–2024)
- Hungary: DK–MSZP–DA (–2024)
- Hungary: LMP (–2026)
- Hungary: PM (–2026)
- Hungary: Párbeszéd (–2024)
- Hungary: United Opposition
- Ireland: Independent Ireland
- Ireland: Independents and others (IND/Neamhspleách)
- Italy: +E (–2024)
- Italy: Art.1
- Italy: Az
- Italy: EV
- Italy: Greens and Left Alliance (Alleanza Verdi e Sinistra)
- Italy: IC
- Italy: IV
- Italy: IV (–2022)
- Italy: IV (–2024)
- Italy: Italexit (–2024)
- Italy: NM
- Italy: National Future (Futuro Nazionale)
- Italy: SI
- Italy: SUE
- Lithuania: LRP
- Lithuania: LT
- Lithuania: Liberals' Movement (Liberalų sąjūdis)
- Lithuania: NS
- Netherlands: 50+
- Netherlands: BIJ1 (–2023)
- Netherlands: GL (–2023)
- Netherlands: Progressive Netherlands (Progressief Nederland)
- Netherlands: Right Answer 2021 (Juiste Antwoord 2021)
- Poland: AU
- Poland: Agreement
- Poland: Civic Coalition (Koalicja Obywatelska (PO, .N and smaller parties))
- Poland: Confederation of the Polish Crown (Konfederacja Korony Polskiej)
- Poland: KP
- Poland: Konfederacja
- Poland: Kukiz'15
- Poland: Lewica
- Poland: P2050
- Poland: TD (–2025)
- Portugal: Democratic Alliance (Aliança Democrática)
- Portugal: L
- Romania: ADU (–2024)
- Romania: PER
- Romania: REPER
- Romania: SENS
- Slovakia: D
- Slovakia: Dobrá voľba
- Slovakia: HA
- Slovakia: OĽaNO & Friends (electoral coalition) (OĽaNO a priatelia (koalícia))
- Slovakia: Vlasť
- Slovakia: ZL (–2023)
- Slovakia: ĽSNS
- Spain: AA
- Spain: CUP (–2023)
- Spain: Cs (–2023)
- Spain: JuntsxCat
- Spain: Más País
- Spain: Na+
- Spain: PACMA (–2023)
- Sweden: Centre Party (Centerpartiet)
- Sweden: L
- Sweden: Swedish Social Democratic Party (Socialdemokraterna)
- UK: Plaid
- UK: RB
- UK: Reform UK (Brexit Party (relaunched as Reform UK in 2021))
- UK: UKIP (–2024)
