
plant_family[plant_family$query ==  c("Pectis") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Kandetia") & is.na(plant_family$family),3]="Rhizophoraceae"
plant_family[plant_family$query ==  c("Maackia") & is.na(plant_family$family),3]="Fabaceae"
plant_family[plant_family$query ==  c("VVedelia") & is.na(plant_family$family),3]="Asteraceae"
#Wrong spelling of Wedelia
plant_family[plant_family$query ==  c("VVedelia") & is.na(plant_family$family),3]="Cucurbitaceae"
plant_family[plant_family$query ==  c("Scorzonea") & is.na(plant_family$family),3]="Asteraceae"
#Wrong spelling of Scorzonera
plant_family[plant_family$query ==  c("Noechamaelea") & is.na(plant_family$family),3]="Rutaceae"
plant_family[plant_family$query ==  c("Benthamidia") & is.na(plant_family$family),3]="Cornaceae"
plant_family[plant_family$query ==  c("Lindera") & is.na(plant_family$family),3]="Lauraceae"
plant_family[plant_family$query ==  c("Besella") & is.na(plant_family$family),3]="Basellaceae"
plant_family[plant_family$query ==  c("Liriope") & is.na(plant_family$family),3]="Asparagaceae"
plant_family[plant_family$query ==  c("Mosta") & is.na(plant_family$family),3]="Unresolved"
plant_family[plant_family$query ==  c("Limophila") & is.na(plant_family$family),3]="Plantaginaceae"
plant_family[plant_family$query ==  c("Alium") & is.na(plant_family$family),3]="Amaryllidaceae"
plant_family[plant_family$query ==  c("Osterium") & is.na(plant_family$family),3]="Apiaceae"
#Ostericum
plant_family[plant_family$query ==  c("Equium") & is.na(plant_family$family),3]="Boraginaceae"
#Echium
plant_family[plant_family$query ==  c("Agauria") & is.na(plant_family$family),3]="Ericaceae"
plant_family[plant_family$query ==  c("Phyllanthus") & is.na(plant_family$family),3]="Phyllanthaceae"
plant_family[plant_family$query ==  c("Ilysanthes") & is.na(plant_family$family),3]="Linderniaceae"
plant_family[plant_family$query ==  c("Dianthera") & is.na(plant_family$family),3]="Acanthaceae"
plant_family[plant_family$query ==  c("Ludvigia") & is.na(plant_family$family),3]="Onagraceae"
plant_family[plant_family$query ==  c("Specularia") & is.na(plant_family$family),3]="Campanulaceae"
plant_family[plant_family$query ==  c("Actinomeris") & is.na(plant_family$family),3]="Asteraceae"
plant_family[plant_family$query ==  c("Psedera") & is.na(plant_family$family),3]="Vitaceae"
plant_family[plant_family$query ==  c("Anemonella") & is.na(plant_family$family),3]="Ranunculaceae"
plant_family[plant_family$query ==  c("Asiminia") & is.na(plant_family$family),3]="Annonaceae"
plant_family[plant_family$query ==  c("Fridericia") & is.na(plant_family$family),3]="Bignoniaceae"
plant_family[plant_family$query ==  c("Centropogon") & is.na(plant_family$family),3]="Campanulaceae"
plant_family[plant_family$query ==  c("Payparola") & is.na(plant_family$family),3]="iolaceae"
plant_family[plant_family$query ==  c("Symphonia") & is.na(plant_family$family),3]="Clusiaceae"

#Number 3 from the bottom

plant_family[plant_family$query ==  c("Tabernamontana") & is.na(plant_family$family),3]="Loganiaceae"
plant_family[plant_family$query ==  c("Pslicourea") & is.na(plant_family$family),3]="Rubiaceae"
#Palicourea
plant_family[plant_family$query ==  c("Desfontainea") & is.na(plant_family$family),3]="Columelliaceae"
#Desfontainia
plant_family[plant_family$query ==  c("Pitcaimia") & is.na(plant_family$family),3]="romeliaceae"
plant_family[plant_family$query ==  c("Osmorrhiza") & is.na(plant_family$family),3]="Apiaceae"
plant_family[plant_family$query ==  c("Ascerates") & is.na(plant_family$family),3]="Apocynaceae"


