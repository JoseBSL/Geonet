unique(plant_family$family)
library(taxize)
plant_order=tax_name(query=unique(plant_family$family),
          get=c("order"),db="ncbi", 
                    division_filter = "Plantae",rank_query="Family")

