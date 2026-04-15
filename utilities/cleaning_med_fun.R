################################################################################

###Auxiliary functions

################################################################################

#####Add ids
##Ivy plus
###Define ivy_plus_ids
ivy_plus_ids = c(
  "https://openalex.org/I136199984",  # Harvard University
  "https://openalex.org/I32971472",  # Yale University
  "https://openalex.org/I20089843",  # Princeton University
  "https://openalex.org/I78577930",  # Columbia University
  "https://openalex.org/I79576946",  # University of Pennsylvania
  "https://openalex.org/I205783295",  # Cornell University
  "https://openalex.org/I27804330",  # Brown University
  "https://openalex.org/I107672454",  # Dartmouth College
  "https://openalex.org/I193597176", # Stanford University
  "https://openalex.org/I63966007", # MIT
  "https://openalex.org/I170897317", # duke
  "https://openalex.org/I40347166" # chicago
  
)

###Oxbridge ids

oxbridge_ids = c("https://openalex.org/I241749" # cambridge
                 , "https://openalex.org/I40120149") # oxford)


# Helpers (0/1 com NA -> 0)
flag_le      <- function(v, k) as.integer(!is.na(v) & v <= k)
flag_between <- function(v, lo, hi) as.integer(!is.na(v) & v <= hi & v > lo)

make_shanghai_flags_dt <- function(DT, suffix = "") {
  stopifnot(inherits(DT, "data.table"))
  
  rk      <- paste0("shanghai_Rank", suffix)
  rk_math <- paste0("shanghaiMath_Rank", suffix)
  rk2003  <- paste0("shanghai_Rank_2003", suffix)
  p_col   <- paste0("phd_university_rank", suffix)
  oa_col  <- paste0("OA_id", suffix)
  oa_phd_col <- paste0("OA_phd_id", suffix)
  iso3_oa_alt_col <- paste0("iso3_oa_alt", suffix)
  iso3_oa_phd_alt_col <- paste0("iso3_oa_phd_alt", suffix)
  oa_display_col <- paste0("OA_display_name", suffix)
  
  
  # Normaliza tipo numérico quando existir
  if (rk %in% names(DT))      DT[, (rk)      := as.numeric(get(rk))]
  if (rk_math %in% names(DT)) DT[, (rk_math) := as.numeric(get(rk_math))]
  if (rk2003 %in% names(DT))  DT[, (rk2003)  := as.numeric(get(rk2003))]
  if (p_col %in% names(DT))   DT[, (p_col)   := as.numeric(get(p_col))]
  
  # ============== 2023 (geral) ==============
  if (rk %in% names(DT)) {
    DT[, c(
      paste0("top10shanghai", suffix),      paste0("outsideTop10shanghai", suffix),
      paste0("top20shanghai", suffix),      paste0("top25shanghai", suffix),
      paste0("top30shanghai", suffix),      paste0("top50shanghai", suffix),
      paste0("outsideTop50shanghai", suffix),
      paste0("top100shanghai", suffix),     paste0("outsideTop100shanghai", suffix),
      paste0("top200shanghai", suffix),     paste0("top500shanghai", suffix),
      paste0("outsideTop500shanghai", suffix), paste0("top300shanghai", suffix),
      paste0("top1kshanghai", suffix),      paste0("outsideTop1kshanghai", suffix),
      paste0("top11to50shanghai", suffix),  paste0("top11to500shanghai", suffix),
      paste0("top51to500shanghai", suffix), paste0("top101to1kshanghai", suffix),
      paste0("top51to1kshanghai", suffix)
    ) := {
      x <- get(rk)
      v10  <- flag_le(x, 10L);  v20  <- flag_le(x, 20L);  v25  <- flag_le(x, 25L)
      v30  <- flag_le(x, 30L);  v50  <- flag_le(x, 50L);  v100 <- flag_le(x, 100L)
      v200 <- flag_le(x, 200L); v300 <- flag_le(x, 300L); 
      v500 <- flag_le(x, 500L); v1k  <- as.integer(!is.na(x))
      list(
        v10,  1L - v10,
        v20,  v25,  v30,
        v50,  1L - v50,
        v100, 1L - v100,
        v200, v300 ,v500, 1L - v500,
        v1k,  1L - v1k,
        flag_between(x, 10L,  50L),
        flag_between(x, 10L, 500L),
        flag_between(x, 50L, 500L),
        flag_between(x, 100L, 1000L),   # top101to1kshanghai
        flag_between(x,  50L, 1000L)    # top51to1kshanghai
      )
    }]
  }
  
  # ============== 2023 (Math) ==============
  if (rk_math %in% names(DT)) {
    DT[, c(
      paste0("top10shanghaiMath", suffix),        paste0("outsideTop10shanghaiMath", suffix),
      paste0("top20shanghaiMath", suffix),        paste0("top25shanghaiMath", suffix),
      paste0("top30shanghaiMath", suffix),        paste0("top50shanghaiMath", suffix),
      paste0("outsideTop50shanghaiMath", suffix),
      paste0("top100shanghaiMath", suffix),       paste0("outsideTop100shanghaiMath", suffix),
      paste0("top200shanghaiMath", suffix),       paste0("top300shanghaiMath", suffix),
      paste0("outsideTop300shanghaiMath", suffix),
      paste0("top11to50shanghaiMath", suffix),    paste0("top11to100shanghaiMath", suffix),
      paste0("top51to100shanghaiMath", suffix),   paste0("top101to300shanghaiMath", suffix),
      paste0("top51to300shanghaiMath", suffix)    # << adicionado
    ) := {
      y <- get(rk_math)
      v10  <- flag_le(y, 10L);  v20  <- flag_le(y, 20L);  v25  <- flag_le(y, 25L)
      v30  <- flag_le(y, 30L);  v50  <- flag_le(y, 50L);  v100 <- flag_le(y, 100L)
      v200 <- flag_le(y, 200L); v300 <- flag_le(y, 300L)
      list(
        v10,  1L - v10,
        v20,  v25,  v30,
        v50,  1L - v50,
        v100, 1L - v100,
        v200, v300, 1L - v300,
        flag_between(y, 10L,  50L),
        flag_between(y, 10L, 100L),
        flag_between(y, 50L, 100L),
        flag_between(y, 100L, 300L),
        flag_between(y,  50L, 300L)     # top51to300shanghaiMath
      )
    }]
  }
  
  # Helper seguro (coluna -> vetor); evita erro quando a coluna ainda não existe
  g <- function(base) {
    nm <- paste0(base, suffix)
    if (nm %in% names(DT)) DT[[nm]] else rep.int(0L, nrow(DT))
  }
  
  # ============== Agregados 2023 ==============
  DT[, (paste0("top_50_uni",  suffix)) := as.integer(g("top50shanghai")    == 1L | g("top50shanghaiMath")   == 1L)]
  DT[, (paste0("top_500_uni", suffix)) := as.integer(g("top500shanghai")   == 1L | g("top300shanghaiMath")  == 1L)]
  DT[, (paste0("top_1k_uni",  suffix)) := as.integer(g("top1kshanghai")    == 1L | g("top300shanghaiMath")  == 1L)]
  
  DT[, (paste0("top_10_uni",  suffix)) := as.integer(g("top10shanghai")    == 1L | g("top10shanghaiMath")   == 1L)]
  DT[, (paste0("top_20_uni",  suffix)) := as.integer(g("top20shanghai")    == 1L | g("top20shanghaiMath")   == 1L)]
  DT[, (paste0("top_25_uni",  suffix)) := as.integer(g("top25shanghai")    == 1L | g("top25shanghaiMath")   == 1L)]
  DT[, (paste0("top_30_uni",  suffix)) := as.integer(g("top30shanghai")    == 1L | g("top30shanghaiMath")   == 1L)]
  DT[, (paste0("top_100_uni", suffix)) := as.integer(g("top100shanghai")   == 1L | g("top100shanghaiMath")  == 1L)]
  DT[, (paste0("top_200_uni", suffix)) := as.integer(g("top200shanghai")   == 1L | g("top200shanghaiMath")  == 1L)]
  DT[, (paste0("top_300_uni", suffix)) := as.integer(g("top300shanghai")   == 1L | g("top300shanghaiMath")  == 1L)]
  
  DT[, (paste0("top_51_to_1k_uni",   suffix)) := as.integer(get(paste0("top_1k_uni",  suffix)) == 1L & get(paste0("top_50_uni",  suffix)) == 0L)]
  DT[, (paste0("top_11_to_1k_uni",   suffix)) := as.integer(get(paste0("top_1k_uni",  suffix)) == 1L & get(paste0("top_10_uni",  suffix)) == 0L)]
  DT[, (paste0("top_101_to_1k_uni",  suffix)) := as.integer(get(paste0("top_1k_uni",  suffix)) == 1L & get(paste0("top_100_uni", suffix)) == 0L)]
  DT[, (paste0("top_301_to_1k_uni",  suffix)) := as.integer(get(paste0("top_1k_uni",  suffix)) == 1L & get(paste0("top_300_uni", suffix)) == 0L)]
  
  DT[, (paste0("top_11_to_50_uni",   suffix)) := as.integer(get(paste0("top_10_uni",  suffix)) == 0L & get(paste0("top_50_uni",  suffix)) == 1L)]
  DT[, (paste0("top_51_to_100_uni",  suffix)) := as.integer(get(paste0("top_100_uni", suffix)) == 1L & get(paste0("top_50_uni",  suffix)) == 0L)]
  DT[, (paste0("top_101_to_300_uni", suffix)) := as.integer(get(paste0("top_300_uni", suffix)) == 1L & get(paste0("top_100_uni", suffix)) == 0L)]
  DT[, (paste0("top_101_to_500_uni", suffix)) := as.integer(get(paste0("top_500_uni", suffix)) == 1L & get(paste0("top_100_uni", suffix)) == 0L)]
  DT[, (paste0("top_201_to_500_uni", suffix)) := as.integer(get(paste0("top_500_uni", suffix)) == 1L & get(paste0("top_200_uni", suffix)) == 0L)]
  DT[, (paste0("top_51_to_200_uni", suffix)) := as.integer(get(paste0("top_200_uni", suffix)) == 1L & get(paste0("top_50_uni", suffix)) == 0L)]
  DT[, (paste0("top_51_to_300_uni",  suffix)) := as.integer(get(paste0("top_300_uni", suffix)) == 1L & get(paste0("top_50_uni",  suffix)) == 0L)]
  DT[, (paste0("top_51_to_500_uni",  suffix)) := as.integer(get(paste0("top_500_uni", suffix)) == 1L & get(paste0("top_50_uni",  suffix)) == 0L)]
  DT[, (paste0("top_501_to_1k_uni",  suffix)) := as.integer(get(paste0("top_1k_uni",  suffix)) == 1L & get(paste0("top_500_uni", suffix)) == 0L)]
  
  # ============== PhD (faixas) ==============
  if (p_col %in% names(DT)) {
    DT[, c(
      paste0("top_10_phd",  suffix),
      paste0("top_50_phd",  suffix),
      paste0("top_100_phd", suffix),
      paste0("top_300_phd", suffix),
      paste0("top_200_phd", suffix),
      paste0("top_500_phd", suffix),
      paste0("top_1k_phd", suffix)
    ) := {
      z <- get(p_col)
      list(
        flag_le(z, 10L), 
        flag_le(z, 50L), flag_le(z, 100L), flag_le(z, 200L) ,flag_le(z, 300L), flag_le(z, 500L), 
        flag_le(z, 1000L)
      )
    }]
    
    DT[, (paste0("top_11_to_50_phd",  suffix)) := as.integer(get(paste0("top_10_phd",  suffix)) == 0L &
                                                                    get(paste0("top_50_phd",  suffix)) == 1L)]
    DT[, (paste0("top_11_to_500_phd", suffix)) := as.integer(get(paste0("top_500_phd", suffix)) == 1L &
                                                                    get(paste0("top_10_phd",  suffix)) == 0L)]
    DT[, (paste0("top_51_to_100_phd", suffix)) := as.integer(get(paste0("top_100_phd", suffix)) == 1L &
                                                                    get(paste0("top_50_phd",  suffix)) == 0L)]
    DT[, (paste0("top_101_to_300_phd", suffix)) := as.integer(get(paste0("top_300_phd", suffix)) == 1L &
                                                                     get(paste0("top_100_phd", suffix)) == 0L)]
    DT[, (paste0("top_101_to_500_phd", suffix)) := as.integer(get(paste0("top_500_phd", suffix)) == 1L &
                                                                     get(paste0("top_100_phd", suffix)) == 0L)]
    DT[, (paste0("top_51_to_200_phd", suffix)) := as.integer(get(paste0("top_200_phd", suffix)) == 1L &
                                                                    get(paste0("top_50_phd",  suffix)) == 0L)]
    DT[, (paste0("top_51_to_300_phd", suffix))  := as.integer(get(paste0("top_300_phd", suffix)) == 1L &
                                                                     get(paste0("top_50_phd",  suffix)) == 0L)]
    DT[, (paste0("top_51_to_500_phd", suffix))  := as.integer(get(paste0("top_500_phd", suffix)) == 1L &
                                                                     get(paste0("top_50_phd",  suffix)) == 0L)]
    DT[, (paste0("top_201_to_500_phd", suffix))  := as.integer(get(paste0("top_500_phd", suffix)) == 1L &
                                                                 get(paste0("top_200_phd",  suffix)) == 0L)]
    
    DT[, (paste0("top_301_to_500_phd", suffix))  := as.integer(get(paste0("top_500_phd", suffix)) == 1L &
                                                                get(paste0("top_300_phd",  suffix)) == 0L)]
    DT[, (paste0("top_51_to_1k_phd", suffix))  := as.integer(get(paste0("top_1k_phd", suffix)) == 1L &
                                                                 get(paste0("top_50_phd",  suffix)) == 0L)]
    
    }
  
  # ============== 2003 (shanghai_Rank_2003{suffix}) ==============
  if (rk2003 %in% names(DT)) {
    DT[, c(
      paste0("top_10_uni_2003",  suffix),
      paste0("top_20_uni_2003",  suffix),
      paste0("top_25_uni_2003",  suffix),
      paste0("top_30_uni_2003",  suffix),
      paste0("top_50_uni_2003",  suffix),
      paste0("top_100_uni_2003", suffix),
      paste0("top_300_uni_2003", suffix),
      paste0("top_200_uni_2003", suffix),
      paste0("top_500_uni_2003", suffix)
    ) := {
      z <- get(rk2003)
      list(
        flag_le(z, 10L), flag_le(z, 20L), flag_le(z, 25L), flag_le(z, 30L),
        flag_le(z, 50L), flag_le(z, 100L), flag_le(z, 300L), flag_le(z, 200L), flag_le(z, 500L)
      )
    }]
    
    DT[, (paste0("top_11_to_50_uni_2003",  suffix)) := as.integer(get(paste0("top_10_uni_2003",  suffix)) == 0L &
                                                                    get(paste0("top_50_uni_2003",  suffix)) == 1L)]
    DT[, (paste0("top_11_to_500_uni_2003", suffix)) := as.integer(get(paste0("top_500_uni_2003", suffix)) == 1L &
                                                                    get(paste0("top_10_uni_2003",  suffix)) == 0L)]
    DT[, (paste0("top_51_to_100_uni_2003", suffix)) := as.integer(get(paste0("top_100_uni_2003", suffix)) == 1L &
                                                                    get(paste0("top_50_uni_2003",  suffix)) == 0L)]
    DT[, (paste0("top_51_to_200_uni_2003", suffix)) := as.integer(get(paste0("top_200_uni_2003", suffix)) == 1L &
                                                                    get(paste0("top_50_uni_2003",  suffix)) == 0L)]
    DT[, (paste0("top_101_to_300_uni_2003", suffix)) := as.integer(get(paste0("top_300_uni_2003", suffix)) == 1L &
                                                                     get(paste0("top_100_uni_2003", suffix)) == 0L)]
    DT[, (paste0("top_201_to_500_uni_2003", suffix)) := as.integer(get(paste0("top_500_uni_2003", suffix)) == 1L &
                                                                     get(paste0("top_200_uni_2003", suffix)) == 0L)]
    DT[, (paste0("top_101_to_500_uni_2003", suffix)) := as.integer(get(paste0("top_500_uni_2003", suffix)) == 1L &
                                                                     get(paste0("top_100_uni_2003", suffix)) == 0L)]
    DT[, (paste0("top_51_to_300_uni_2003", suffix))  := as.integer(get(paste0("top_300_uni_2003", suffix)) == 1L &
                                                                     get(paste0("top_50_uni_2003",  suffix)) == 0L)]
    DT[, (paste0("top_51_to_500_uni_2003", suffix))  := as.integer(get(paste0("top_500_uni_2003", suffix)) == 1L &
                                                                     get(paste0("top_50_uni_2003",  suffix)) == 0L)]
    DT[, (paste0("top_21_to_500_uni_2003", suffix))  := as.integer(get(paste0("top_500_uni_2003", suffix)) == 1L &
                                                                     get(paste0("top_20_uni_2003",  suffix)) == 0L)]
    DT[, (paste0("top_31_to_500_uni_2003", suffix))  := as.integer(get(paste0("top_500_uni_2003", suffix)) == 1L &
                                                                     get(paste0("top_30_uni_2003",  suffix)) == 0L)]
  }
  
  # ============== BAFound / BAnotfound ==============
  oa_col <- paste0("OA_id", suffix)
  if (oa_col %in% names(DT)) {
    v  <- DT[[oa_col]]
    # trate "" como ausente quando OA_id for character; ajuste se quiser
    miss <- is.na(v) | (is.character(v) & v == "")
    ba   <- as.integer(!miss)
  } else {
    ba <- rep.int(0L, nrow(DT))
  }
  DT[, c(paste0("BAFound", suffix), paste0("BAnotFound", suffix)) := .(ba, 1L - ba)]
  

  
  # ============== PhDFound / PhDnotfound ==============
  oa_phd_col <- paste0("OA_id_phd", suffix)  # troque aqui se o nome for outro
  if (oa_phd_col %in% names(DT)) {
    vp   <- DT[[oa_phd_col]]
    miss <- is.na(vp) | (is.character(vp) & vp == "")
    phd_found <- as.integer(!miss)
  } else {
    phd_found <- rep.int(0L, nrow(DT))
  }
  DT[, c(paste0("PhDFound", suffix),
         paste0("PhDNotFound", suffix),
         paste0("phd", suffix)) := .(phd_found, 1L - phd_found, phd_found)]
  
  # ============== Novas variáveis ==============
  # ivy_plus (usa um vetor global `ivy_plus_ids`)
  DT[, (paste0("ivy_plus", suffix)) := fifelse(is.na(get(oa_col)), 0L, as.integer(get(oa_col) %in% ivy_plus_ids))]
  
  # oxbridge
  DT[, (paste0("oxbridge", suffix)) := fifelse(is.na(get(oa_col)), 0L, as.integer(get(oa_col) %in% oxbridge_ids))]
  
  # ---------- UGmigrant ----------
  if (all(c(iso3_oa_alt_col, "iso3_o_alt") %in% names(DT))) {
    ug_alt  <- DT[[iso3_oa_alt_col]]
    origin  <- DT[["iso3_o_alt"]]   # sempre sem sufixo
    DT[, (paste0("UGmigrant", suffix)) :=
         fifelse(!is.na(ug_alt) & !is.na(origin) & ug_alt != origin, 1L, 0L, na = 0L)]
  } else {
    DT[, (paste0("UGmigrant", suffix)) := 0L]
  }
  
  # ---------- Phdmigrant ----------
  # compara país do PhD (alt) vs origem (fixo em iso3_o_alt)
  if (all(c(iso3_oa_phd_alt_col, "iso3_o_alt") %in% names(DT))) {
    phd_alt <- DT[[iso3_oa_phd_alt_col]]
    origin  <- DT[["iso3_o_alt"]]
    DT[, (paste0("Phdmigrant", suffix)) :=
         fifelse(!is.na(phd_alt) & !is.na(origin) & phd_alt != origin, 1L, 0L, na = 0L)]
  } else {
    DT[, (paste0("Phdmigrant", suffix)) := 0L]
  }
  
  # ---------- toUS / toUK / toUSorUK ----------
  if (iso3_oa_alt_col %in% names(DT)) {
    x <- DT[[iso3_oa_alt_col]]
    DT[, (paste0("toUS_UG", suffix))     := fifelse(!is.na(x) & x == "USA", 1L, 0L)]
    DT[, (paste0("toUK_UG", suffix))     := fifelse(!is.na(x) & x == "GBR", 1L, 0L)]
    DT[, (paste0("toUSorUK_UG", suffix)) := fifelse(is.na(x), 0L, as.integer(x %in% c("USA","GBR")))]
  } else {
    DT[, c(paste0("toUS_UG", suffix),
           paste0("toUK_UG", suffix),
           paste0("toUSorUK_UG", suffix)) := .(0L, 0L, 0L)]
  }
  
  # ---------- MIT / Cambridge ----------
  disp_col <- if (exists("oa_display_col") && oa_display_col %in% names(DT)) {
    oa_display_col
  } else if ("OA_display_name" %in% names(DT)) {
    "OA_display_name"
  } else {
    NA_character_
  }
  
  if (!is.na(disp_col) && oa_col %in% names(DT)) {
    disp <- DT[[disp_col]]
    oid  <- DT[[oa_col]]
    DT[, (paste0("MITorCambridge", suffix)) :=
         fifelse(is.na(oid), 0L,
                 as.integer(!is.na(disp) &
                              disp %in% c("Massachusetts Institute of Technology",
                                          "University of Cambridge")))]
    DT[, (paste0("MIT", suffix)) :=
         fifelse(is.na(oid), 0L,
                 as.integer(!is.na(disp) & disp == "Massachusetts Institute of Technology"))]
    DT[, (paste0("Cambridge", suffix)) :=
         fifelse(is.na(oid), 0L,
                 as.integer(!is.na(disp) & disp == "University of Cambridge"))]
  } else {
    DT[, c(paste0("MITorCambridge", suffix),
           paste0("MIT", suffix),
           paste0("Cambridge", suffix)) := .(0L, 0L, 0L)]
  }
  
  # ---------- stayedForPhd ----------
  if (all(c(oa_col, oa_phd_col) %in% names(DT))) {
    id_ba  <- DT[[oa_col]]
    id_phd <- DT[[oa_phd_col]]
    DT[, (paste0("stayedForPhd", suffix)) :=
         fifelse(is.na(id_ba) | is.na(id_phd), 0L, as.integer(id_phd == id_ba))]
  } else {
    DT[, (paste0("stayedForPhd", suffix)) := 0L]
  }
  
  # ---------- min rank (geral vs math) ----------
  x <- if (rk      %in% names(DT)) DT[[rk]]      else rep(NA_real_, nrow(DT))
  y <- if (rk_math %in% names(DT)) DT[[rk_math]] else rep(NA_real_, nrow(DT))
  
  # menor rank entre geral e math (NA se ambos forem NA)
  min_any <- ifelse(is.na(x) & is.na(y), NA_real_, pmin(x, y, na.rm = TRUE))
  DT[, (paste0("Top_Rank", suffix)) := min_any]
  
  invisible(DT)
}


apply_shanghai_groups <- function(DT, suffixes = c("", "_MI", "_lk")) {
  for (s in suffixes) make_shanghai_flags_dt(DT, s)
  invisible(DT)
}