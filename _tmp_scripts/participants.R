data_raw_participants <- structure(list(V1 = c(
  "Appels", "Bogatsu", "Boorman", "Chiddy",
  "Fastovsky", "Goosen", "Impson", "Lamba", "Leibrandt", "Leicher",
  "Lin", "Linakane", "Little", "Mafereka", "Makanjee", "Makgolane",
  "Mathavhathe", "Mbuyazi", "Mpurwana", "Munatsi", "Mwanawina",
  "Osman", "Phiri", "Plaatjies", "Rowell", "Senderayi", "Stead",
  "Van Der Merwe", "Van Huyssteen", "Walbeck", "White", "Whittaker"
), V2 = c(
  " Noeriniche", " Siphiwe", " Kyle", " Ben", " Anna",
  " Connor", " Olivia", " Taffy", " Leith", " Marc", " Zhengyu",
  " Keke", " Thomas", " Tlotliso", " Praveer", " Phemelo", " Ompha",
  " Billy", " Sinovuyo", " Chrysanthemum", " Sanana", " Mohammed",
  " Mishan", " Zusakhe", " Dylan", " Maxine", " Peter", " Georgie",
  " Ben", " Paul", " David", " Alexandra"
), V3 = c(
  " appnoe002",
  " bgtsim001", " brmkyl002", " chdben002", " fstann002", " gsncon001",
  " impoli001", " lmbtaf001", " lbrlei001", " lchmar008", " lnxzhe001",
  " lnkkek001", " ltttho003", " mfrtlo001", " mknpra006", " mkglet008",
  " mthomp006", " mbybil002", " mpryon001", " mntchr005", " mwnsan002",
  " osmmoh022", " phrmis001", " pltnya001", " nlsdyl002", " sndmax001",
  " stdpet001", " vmrgeo003", " vhyben001", " wlbpau001", " whtdav023",
  " whtale012"
), V4 = c(
  " APPNOE002@myuct.ac.za", " BGTSIM001@myuct.ac.za",
  " BRMKYL002@myuct.ac.za", " CHDBEN002@myuct.ac.za", " FSTANN002@myuct.ac.za",
  " GSNCON001@myuct.ac.za", " IMPOLI001@myuct.ac.za", " LMBTAF001@myuct.ac.za",
  " LBRLEI001@myuct.ac.za", " LCHMAR008@myuct.ac.za", " LNXZHE001@myuct.ac.za",
  " LNKKEK001@myuct.ac.za", " LTTTHO003@myuct.ac.za", " MFRTLO001@myuct.ac.za",
  " MKNPRA006@myuct.ac.za", " MKGLET008@myuct.ac.za", " MTHOMP006@myuct.ac.za",
  " MBYBIL002@myuct.ac.za", " MPRYON001@myuct.ac.za", " MNTCHR005@myuct.ac.za",
  " MWNSAN002@myuct.ac.za", " OSMMOH022@myuct.ac.za", " PHRMIS001@myuct.ac.za",
  " PLTNYA001@myuct.ac.za", " NLSDYL002@myuct.ac.za", " SNDMAX001@myuct.ac.za",
  " STDPET001@myuct.ac.za", " VMRGEO003@myuct.ac.za", " VHYBEN001@myuct.ac.za",
  " WLBPAU001@myuct.ac.za", " WHTDAV023@myuct.ac.za", " WHTALE012@myuct.ac.za"
)), class = "data.frame", row.names = c(NA, -32L))

set.seed(2106)
data_raw_participants |>
  tibble::as_tibble() |>
  dplyr::mutate(dplyr::across(everything(), function(x) gsub(" ", "", x))) |>
  dplyr::select(V2, V1, V4) |>
  dplyr::rename(
    `Last name` = V1,
    `First name` = V2,
    `Email address` = V4
  ) |>
  dplyr::mutate(
    group = rep(sample(1:16, size = 16), each = 2)
  )
