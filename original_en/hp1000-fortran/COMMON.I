
      COMMON/MMBC/IVRB(4,89),IRNM(8,56),IRSN(4,17),IPRP(4,17),
     1IDTN(4,11),ICLS(4,12),RND,IWRD(9,9),IPR(5),
     2IROM(99),IRES(17),IVEN(107),IXT(360),ITST(40),NAME(3),NTAG(10),
     3IWP(4)

      common/mmseg/is_sa, is_sb, is_sc, is_sd, is_se, is_sf, is_sg,
     1 is_sh, is_si, is_sj, is_sk, is_sl
	
      complex cvrb(89)
      character*18 chwrd(9)
      equivalence(ivrb, cvrb)
