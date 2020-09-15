#carregar biblioteca
library(fipe)


#carros e marcas de carros para puxar dados
#carros mais vendidos populares
carros = list("Etios", "Ka", "kwid",
           "uno","hb20",
           "gol","onix","sandero",
           "grand siena","argo",
           "march", "up","qq",
           "mobi","530","fox","up",
           "celer","polo")

marca = list("toyota", "ford", "renault",
        "fiat","hyundai","vw - volkswagen",
         "gm - chevrolet","renault","fiat","fiat","nissan",
         "vw - volkswagen","chery","fiat","lifan",
         "vw - volkswagen","vw - volkswagen","chery",
        "vw - volkswagen")

#Pegando dados pela api
data_202009 <- purrr::pmap(list(carros[1:2], marca[1:2]), function(carros,marca) {
  fipe_vehicle(carros, marca, c(2014,2015,2016,2017,2018,2019,0) )} )

#salvar dados
readr::write_rds(dplyr::bind_rows(data_202009),
                 "./data/2020_09_fipe_top_veiculos.rds",
                 compress = "gz")



