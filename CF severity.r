#severity of genotype

# Red = severe or minimal function mutation
# Orange= unknown mutation status (for now) 
# Yellow= partial function or mild
# Green = not CF causing, classify as "other", will most likely be seen in people with CRMS diagnosis


#
data_0$class2 <- ifelse(data_0$Genotypes2 %in% c('1717-1G->A', '1717-1G-A',  'G542X', 'Q493X', 'R1162X', 'R553X', 'R553x', 'W1089X', 'W1282X', '1078delT', 'R75X',
                                      '3659delC', '621+1G->T', '621+1G>T', '394delTT', '3120 + 1g->A', '1154InsTC',  '1154insTC', '1213delT',
                                      '1259insA', '1288insTA', '3791delC', 'E60X', 'K710X', '2184delA', 'CFTRdele2,3', '663delT', 'Glu528',
                                      '1461ins4', '306insA', 'R709X', 'CFTRdele22-24', '2711delT', '2183AA- >G'), "I", 
               ifelse(data_0$Genotypes2 %in% c('F508', 'F508del', 'F508del ', 'G85E', 'I507', 'I 507', 'N1303K', 'A559T', 'R560T', 'A561E'), 
                      "II", 
               ifelse(data_0$Genotypes2 %in% c('G551D', 'G551S', 'S549N'), "III", 
               ifelse(data_0$Genotypes2 %in% c('R334W', 'R347P', 'R117H', 'R117C', 'P67L', 'L206W', 'I206w', 'D614G', 'R347H', 'D1152H', '3849+10kbC>T'), 
                      "IV", 
               ifelse(data_0$Genotypes2 %in% c('A455E', '2789+5G->A', '3849+10CT', '3849+10kbC->T', '711+3A->G', '1898+5G->T',
                                             'P574H', '3272-26A->G5'), "V" , "")))))   
 
data_0$severity2 <- ifelse(data_0$Genotypes2 %in% c('1717-1G->A', '1717-1G-A',  'G542X', 'Q493X', 'R1162X', 'R553X', 'R553x', 'W1089X', 'W1282X', '1078delT', 'R75X',
                                             '3659delC', '621+1G->T', '621+1G>T', '394delTT', '3120 + 1g->A', '1154InsTC',  '1154insTC', '1213delT',
                                             '1259insA', '1288insTA', '3791delC', 'E60X', 'K710X', '2184delA', 'CFTRdele2,3', '663delT', 'Glu528',
                                             '1461ins4', '306insA', 'R709X', 'CFTRdele22-24', '2711delT', '2183AA- >G','F508', 'F508del', 'F508del ', 'G85E', 
                                             'I507', 'I 507', 'N1303K', 'A559T', 'R560T', 'A561E','G551D', 'G551S', 'S549N','1898+1G->A', '2143delT', '2183AA-G',
                                             '2183delAA->G', '2184insA', 'S434X',
                                                       '2585delT', '3120+1G->A',  '3905insT',  '406-1G->A', '712-1G->T', 'L1245X', 'Q2X', 'R1158X',
                                                       'R851X', 'V520F', 'L1077P', 'S489X', 'F311del', 'W846X', 'M1101K', '2622+1G->A', 'S945L', 
                                                  "Y1092X", "R1066C", "G178R", "2957delT", "CFTRdele1", 'W1204X', "q493x", "W57G", "1585-8G>A", "1717-8G->A","G1244E"), 
                           "Severe", 
               ifelse(data_0$Genotypes2 %in% c('R334W', 'R347P', 'R117H', 'R117C', 'P67L', 'L206W', 'I206w', 'D614G', 'R347H', 'D1152H', '3849+10kbC>T', "D110H", "R1070W",
                                               'A455E', '2789+5G->A', '3849+10CT', '3849+10kbC->T', '711+3A->G', '1898+5G->T',
                                                                         'P574H', '3272-26A->G5','3849G->A', 'H199Y', 'H199y',  'S492F', 'S531P',
                                               'L1335P', 'R352Q', "D1270N", "2789+2insA", "3849+10kbc->T", "Q98R", "Phe1052Val", 
                                               "F1052V", "Tyr1014Cys", "Y1014C", "T338I", "5T", "c.2249C>T", "P750L"), "Mild" ,
                
                       ifelse(data_0$Genotypes2 %in% c(  '1262insA', 'I506T', '3363delGT' , 'Q1209P', '3349insT', 'G178R', 'L467P', '317insC', 'I336K',	
                                                       'ex14a', '3500-2A->G', 'c.3407_3422del16', '3349insT', '1248+1G->A', 'M470V', '1812-1G->A',
                                                       'del X22-24', 'Q237H', 'Q237H', 'W57G', '1078delA', 'G178R', '2957delT', 'G576A',
                                                       'other', 'Other', 'Unknown', 'unknown', 'CFTRdele1', '1811+1.6kb A->', 'UNK', "I336K", "M470V", "S1235R", "M470V", "F508C",
                                                       "R1066H", "P205S"), 
                              "Unk","")))



  
data_0$class1 <- ifelse(data_0$Genotypes1 %in% c('1717-1G->A', '1717-1G-A',  'G542X', 'Q493X', 'R1162X', 'R553X', 'R553x', 'W1089X', 'W1282X', '1078delT', 'R75X',
                                             '3659delC', '621+1G->T', '621+1G>T', '394delTT', '3120 + 1g->A', '1154InsTC',  '1154insTC', '1213delT',
                                             '1259insA', '1288insTA', '3791delC', 'E60X', 'K710X', '2184delA', 'CFTRdele2,3', '663delT', 'Glu528',
                                             '1461ins4', '306insA', 'R709X', 'CFTRdele22-24', '2711delT', '2183AA- >G'), "I", 
                      ifelse(data_0$Genotypes1 %in% c('F508', 'F508del', 'F508del ', 'G85E', 'I507', 'I 507', 'N1303K', 'A559T', 'R560T', 'A561E'), 
                             "II", 
                             ifelse(data_0$Genotypes1 %in% c('G551D', 'G551S', 'S549N'), "III", 
                                    ifelse(data_0$Genotypes1 %in% c('R334W', 'R347P', 'R117H', 'R117C', 'P67L', 'L206W', 'I206w', 'D614G', 'R347H', 'D1152H', '3849+10kbC>T'), 
                                           "IV", 
                                           ifelse(data_0$Genotypes1 %in% c('A455E', '2789+5G->A', '3849+10CT', '3849+10kbC->T', '711+3A->G', '1898+5G->T',
                                                                         'P574H', '3272-26A->G5'), "V" , "")))))   
data_0$severity1 <- ifelse(data_0$Genotypes1 %in% c('1717-1G->A', '1717-1G-A',  'G542X', 'Q493X', 'R1162X', 'R553X', 'R553x', 'W1089X', 'W1282X', '1078delT', 'R75X',
                                                    '3659delC', '621+1G->T', '621+1G>T', '394delTT', '3120 + 1g->A', '1154InsTC',  '1154insTC', '1213delT',
                                                    '1259insA', '1288insTA', '3791delC', 'E60X', 'K710X', '2184delA', 'CFTRdele2,3', '663delT', 'Glu528',
                                                    '1461ins4', '306insA', 'R709X', 'CFTRdele22-24', '2711delT', '2183AA- >G','F508', 'F508del', 'F508del ', 'G85E', 
                                                    'I507', 'I 507', 'N1303K', 'A559T', 'R560T', 'A561E','G551D', 'G551S', 'S549N','1898+1G->A', '2143delT', '2183AA-G',
                                                    '2183delAA->G', '2184insA', 'S434X',
                                                    '2585delT', '3120+1G->A',  '3905insT',  '406-1G->A', '712-1G->T', 'L1245X', 'Q2X', 'R1158X',
                                                    'R851X', 'V520F', 'L1077P', 'S489X', 'F311del', 'W846X', 'M1101K', '2622+1G->A', 'S945L', 
                                                    "Y1092X", "R1066C", "G178R", "2957delT", "CFTRdele1", 'W1204X', "q493x", "W57G", "1585-8G>A", "1717-8G->A","G1244E"), 
                           "Severe", 
                           ifelse(data_0$Genotypes1 %in% c('R334W', 'R347P', 'R117H', 'R117C', 'P67L', 'L206W', 'I206w', 'D614G', 'R347H', 'D1152H', '3849+10kbC>T', "D110H", "R1070W",
                                                           'A455E', '2789+5G->A', '3849+10CT', '3849+10kbC->T', '711+3A->G', '1898+5G->T',
                                                           'P574H', '3272-26A->G5','3849G->A', 'H199Y', 'H199y',  'S492F', 'S531P',
                                                           'L1335P', 'R352Q', "D1270N", "2789+2insA", "3849+10kbc->T", "Q98R", "Phe1052Val", 
                                                           "F1052V", "Tyr1014Cys", "Y1014C", "T338I", "5T", "c.2249C>T", "P750L"), "Mild" ,
                                  
                                  ifelse(data_0$Genotypes1 %in% c(  '1262insA', 'I506T', '3363delGT' , 'Q1209P', '3349insT', 'G178R', 'L467P', '317insC', 'I336K',	
                                                                    'ex14a', '3500-2A->G', 'c.3407_3422del16', '3349insT', '1248+1G->A', 'M470V', '1812-1G->A',
                                                                    'del X22-24', 'Q237H', 'Q237H', 'W57G', '1078delA', 'G178R', '2957delT', 'G576A',
                                                                    'other', 'Other', 'Unknown', 'unknown', 'CFTRdele1', '1811+1.6kb A->', 'UNK', "I336K", "M470V", "S1235R", "M470V", "F508C",
                                                                    "R1066H", "P205S"), 
                                         "Unk","")))


data_0$genoRisk <- ifelse(data_0$severity1 == "Mild" | data_0$severity2 == "Mild", "Low", 
                  ifelse(data_0$severity1 == "Severe" & data_0$severity2 == "Severe", "High", ""))
  
  
                            