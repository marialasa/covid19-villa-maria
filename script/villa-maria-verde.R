library(tidyverse)
library(viridis)
library(ggmap)
library(sf)

radios <- st_read('.../radios.shp')
manzanas <- st_read('.../manzanas.shp')

espacios_verdes <- manzanas %>%
  filter(MVM30_ %in% c(16, 73, 152, 178, 219, 221, 238, 240, 252, 259, 270,
                      276, 298, 301, 313, 325, 332, 339, 363, 391, 405, 410,
                      437, 443, 454, 465, 486, 498, 539, 540, 586, 625, 628,
                      633, 642, 670, 693, 707, 737, 741, 795, 797, 801, 812,
                      833, 839, 848, 859, 860, 863, 867, 872, 873, 876, 879,
                      889, 894, 898, 904, 908, 913, 917, 932, 955, 961, 963,
                      971, 972, 977, 981, 989, 1012, 1032, 1043, 1044, 1053,
                      1095, 1096, 1100, 1108, 1110, 1120, 1138, 1140, 1148,
                      1173, 1175, 1184, 1211, 1223, 1227, 1236, 1237, 1247,
                      1266, 1267, 1273, 1290, 1293, 1311, 1319, 1336, 1341,
                      1344, 1347, 1355, 1364, 1379, 1382, 1391, 1394, 1397,
                      1398, 1413, 1431, 1437, 1438, 1440, 1449, 1469, 1481,
                      1486, 1487, 1489, 1490, 1503, 1504, 1506, 1515, 1529,
                      1573, 1579, 1586, 1671))

radios$POBLACION <- c(932, 1220, 1378, 736, 1290, 1222, 1567, 1283,
                      1361, 980, 585, 922, 848, 1126, 1290, 754, 726,
                      828, 2083, 828, 705, 579, 884, 1037, 543, 925,
                      490, 878, 842, 921, 440, 928, 735, 859, 810,
                      806, 656, 427, 811, 456, 704, 664, 671, 413,
                      971, 718, 740, 770, 1257, 358, 786, 1042, 526,
                      416, 438, 479, 1062, 779, 483, 937, 526, 1055,
                      595, 580, 805, 464, 648, 669, 923, 831, 864, 1697,
                      657, 382, 529, 547, 883, 727, 921, 432, 543, 722,
                      461, 1365, 916, 650, 646, 639, 756, 489, 762, 710,
                      699, 1007, 849, 1461, 1036, 0, 0, 0)

espacios_verdes_c <- st_point_on_surface(espacios_verdes)
radios_c <- st_point_on_surface(radios)

radios_c$POBLACION <- c(932, 1220, 1378, 736, 1290, 1222, 1567, 1283,
                        1361, 980, 585, 922, 848, 1126, 1290, 754, 726,
                        828, 2083, 828, 705, 579, 884, 1037, 543, 925,
                        490, 878, 842, 921, 440, 928, 735, 859, 810,
                        806, 656, 427, 811, 456, 704, 664, 671, 413,
                        971, 718, 740, 770, 1257, 358, 786, 1042, 526,
                        416, 438, 479, 1062, 779, 483, 937, 526, 1055,
                        595, 580, 805, 464, 648, 669, 923, 831, 864, 1697,
                        657, 382, 529, 547, 883, 727, 921, 432, 543, 722,
                        461, 1365, 916, 650, 646, 639, 756, 489, 762, 710,
                        699, 1007, 849, 1461, 1036, 0, 0, 0)

ggplot() +
  geom_sf(data = espacios_verdes, fill = "lightgreen")

ggplot() +
  geom_sf(data = radios_c, size = 0.3) +
  geom_sf(data = radios, fill = NA) +
  geom_sf(data = espacios_verdes, size = 0.3) +
  geom_sf(data = espacios_verdes_c, color = 'lightgreen')

st_nearest_feature(radios_c, espacios_verdes_c)

espacios_verdes_c[st_nearest_feature(radios_c, espacios_verdes_c), ]
nrow(espacios_verdes_c[st_nearest_feature(radios_c, espacios_verdes_c), ])

st_distance(radios_c, espacios_verdes_c[st_nearest_feature(radios_c, espacios_verdes_c),], by_element = TRUE)

radios <- radios %>%
  mutate(distancia = st_distance(radios_c, espacios_verdes_c[st_nearest_feature(radios_c, espacios_verdes_c), ], by_element = TRUE)) %>%
  mutate(distancia = as.numeric(distancia))

mapa_01 <- ggplot() +
  geom_sf(data = radios,
          aes(fill = distancia), color = NA) +
  scale_fill_viridis(option="D", direction = -1) +
  labs(title = "¿Cuánto hay que caminar hasta un espacio verde público?",
       subtitle = "Villa María, Córdoba (AR)",
       caption = "Fuente: http://medium.com/@Condolasa con datos de la Provincia de Córdoba",
       fill = "Metros") +
  theme_test()

mapa_02 <- ggplot() +
  geom_sf(data = filter(radios, distancia > 500),
          aes(fill = distancia), color = NA) +
  scale_fill_viridis_c(option = "D", direction = -1) +
  labs(title = "Radios censales con espacios verdes públicos a más de 500 mts.",
       subtitle = "Villa María, Córdoba (AR)",
       caption = "Fuente: http://medium.com/@Condolasa con datos de la Provincia de Córdoba",
       fill = "Metros") +
  theme_test()

mapa_03 <- ggplot() +
  geom_sf(data = filter(radios, distancia < 500),
          aes(fill = distancia), color = NA) +
  scale_fill_viridis_c(option = "D", direction = -1) +
  labs(title = "Radios censales con espacios verdes públicos a menos de 500 mts.",
       subtitle = "Villa María, Córdoba (AR)",
       caption = "Fuente: http://medium.com/@Condolasa con datos de la Provincia de Córdoba",
       fill = "Metros") +
  theme_test()

radios %>% 
  group_by(distancia > 500) %>% 
  summarise(total = sum (POBLACION),
            pct = total / sum (radios_c$POBLACION))

ggsave(filename = "....png",
       plot = ...,
       dpi = 300)
