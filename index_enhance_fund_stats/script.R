library(quantbox)
library(WindR)
library(stringr)
library(ggplot2)
#w.start(showmenu = FALSE)

#########################************####################################
########################~~ fund return stats ~~######################################
#########################************####################################

begT <- as.Date('2011-01-04')
endT <- trday.nearby(Sys.Date(),-1)
indexID <- c('EI000300','EI000905') # index enhanced fund set

#get all index enhanced funds required
f_basic_info <- w.wset('sectorconstituent',date=endT,'sectorid=2001010103000000')[[2]]
f_basic_info <- f_basic_info %>% filter(!(str_sub(sec_name,start = -1) %in% c('C','E','H'))) %>%
  rename(fundID=wind_code,fundName=sec_name) %>% select(fundID,fundName)


indexID_ <- stockID2stockID(indexID,'local','wind')
ipoday <- w.wss(f_basic_info$fundID,'fund_setupdate,fund_trackindexcode')[[2]]
ipoday <- ipoday %>% filter(FUND_SETUPDATE>0,FUND_TRACKINDEXCODE %in% indexID_) %>% mutate(FUND_SETUPDATE=w.asDateTime(FUND_SETUPDATE, asdate = TRUE)) %>%
  rename(fundID=CODE,setupdate=FUND_SETUPDATE,trackindexcode=FUND_TRACKINDEXCODE)

f_basic_info <- ipoday %>% left_join(f_basic_info,by='fundID')
f_basic_info <- f_basic_info %>% mutate(trackindexcode=stockID2stockID(trackindexcode,'wind','local'))
f_basic_info[f_basic_info$fundID=='310318.OF','setupdate'] <- as.Date('2013-06-08') #fix a bug
f_basic_info[f_basic_info$fundID=='200002.OF','setupdate'] <- as.Date('2011-04-01') #fix a bug

add_fund <- data.frame(fundID=c('001733.OF','000978.OF','001244.OF','460009.OF','003312.OF',
                                '003311.OF','000877.OF','001074.OF'),
                       trackindexcode=c('EI000905','EI000905','EI000905','EI000905','EI000905',
                                             'EI000300','EI000300','EI000300'),stringsAsFactors = FALSE)
add_fund_ <- w.wss(add_fund$fundID,'sec_name,fund_setupdate')[[2]]
add_fund_ <- add_fund_ %>% rename(fundID=CODE,fundName=SEC_NAME,setupdate=FUND_SETUPDATE) %>%
  mutate(setupdate=w.asDateTime(setupdate,asdate = TRUE))
add_fund <- add_fund %>% left_join(add_fund_,by='fundID')	%>%
  select(fundID,setupdate,trackindexcode,fundName)
f_basic_info <- rbind(f_basic_info,add_fund)

#get fund's nav return
fundnav <- MF_getQuote(fundID = f_basic_info$fundID, begT = begT, endT = endT, variables = "NVDailyGrowthRate",datasrc = 'jy')
fundnav <- fundnav %>% mutate(fundID=as.character(fundID),
                              NVDailyGrowthRate=ifelse(is.na(NVDailyGrowthRate),0,NVDailyGrowthRate)) %>% left_join(f_basic_info[,c('fundID','setupdate','trackindexcode')],by='fundID') %>%
  filter(date>=setupdate+months(3)) %>%
  rename(nav_rtn=NVDailyGrowthRate) %>%
  select(-setupdate)

#get benchmark return
indexqt <- getIndexQuote(stocks = indexID, begT = begT, endT = endT, variables ='pct_chg',datasrc = 'jy')
indexqt <- indexqt %>% rename(trackindexcode=stockID) %>%
  mutate(trackindexcode=as.character(trackindexcode))
fundnav <- fundnav %>% left_join(indexqt,by=c('date','trackindexcode')) %>% rename(bmk_rtn=pct_chg) %>% select(-trackindexcode)

#stats of each year
fundstat <- MF_nav_stat(freq = 'year',fundnav=fundnav)
fundstat <- fundstat %>% left_join(f_basic_info,by='fundID')

#stats of total
fundstatol <- MF_nav_stat(fundnav=fundnav)
fundstatol <- fundstatol %>% select(-bench,-bench_ann,-alphaIR,-hitratio,-bias,-TE,
                                    -rtn_min,-rtn_max,-rtn_mindate,-rtn_maxdate,-maxDDbegT,-maxDDendT)

vars <- c('nav_rtn','bmk_rtn')
for(i in 1:length(vars)){
  fundnavts <- reshape2::dcast(fundnav,date~fundID,value.var = vars[i],fill = 0)
  fundnavts <- xts::xts(fundnavts[,-1],order.by = fundnavts[,1])
  fundnavts <- rtn.periods(fundnavts,freq = 'month')
  fundnavts <- head(fundnavts,nrow(fundnavts)-2)
  fundnavts <- reshape2::melt(fundnavts,factorsAsStrings = FALSE)
  colnames(fundnavts) <- c('date','fundID',vars[i])
  if(i==1){
    fundnavmon <- fundnavts
  }else{
    fundnavmon <- left_join(fundnavmon,fundnavts,by=c('date','fundID'))
  }
}
fundnavmon <- fundnavmon %>% filter(nav_rtn!=0)
fundnavmon <- tidyr::separate(fundnavmon,date,c('begT','endT'),sep='~')
fundnavmon <- fundnavmon %>% rename(date=endT) %>%
  mutate(date=as.Date(date)) %>% select(-begT) %>% arrange(date,fundID) %>% arrange(date,fundID)
fundstatmon <- MF_nav_stat(scale = 12,fundnav=fundnavmon)
fundstatmon <- fundstatmon %>% select(fundID,alphaIR,hitratio,
                                    rtn_min,rtn_max,rtn_mindate,rtn_maxdate)

fundstatol <- fundstatol %>% left_join(fundstatmon,by='fundID')
fundstatol <- fundstatol %>% left_join(f_basic_info,by='fundID')

fundstatol <- fundstatol  %>% arrange(trackindexcode,desc(alpha_ann)) %>%
  select(-fundID,-setupdate,-trackindexcode,-alpha) %>% select(fundName,everything())

########################****************#################################
########################~~ random port ~~######################################
########################****************#################################


indexINFO <- data.frame(indexID=c('EI000300','EI000905'),indexAbbr=c('hs300','zz500'),
                        compNUM=c(300,500),selcNUM=c(80,100),stringsAsFactors = FALSE)

wgtTYPEDF <- data.frame(typeName=c('简单随机','简单随机等权','简单随机市值中性','简单随机行业中性','行业中性','行业市值中性','市值中性'),
                        typeAbbr=c('rand','randeq','randsize','randsec','sec','secsize','size'),
                        wgtType=c('ffv','eq','ffv','ffv','optimize','optimize','optimize'),
                        stringsAsFactors = FALSE)

nport <- 10
begT <- as.Date('2011-01-04')
endT <- trday.nearby(Sys.Date(),-1)
RebDates <- getRebDates(begT,endT,'month')
RebDates <- RebDates[1:(length(RebDates)-1)]

randomnav <- data.frame()
totport <- data.frame()

#sector neutral
#multiple factors
factorIDs <- c("F000006")
FactorLists1 <- buildFactorLists_lcfs(factorIDs,
                                      factorRefine=refinePar_default("scale"))
FactorLists2 <- buildFactorLists(
  buildFactorList(factorFun="gf.ln_mkt_cap",
                  factorDir=-1),
  buildFactorList(factorFun="gf.NP_YOY",
                  factorDir=1),
  factorRefine=refinePar_default("scale")
)
FactorLists <- c(FactorLists1,FactorLists2)

for(k in 1:nrow(indexINFO)){
  cat(indexINFO$indexID[k],'...\n')
  TS <- getTS(RebDates,indexINFO$indexID[k])
  TSS <- getSectorID(TS,fillNA = TRUE)
  re <- TSS %>% group_by(date,sector) %>% summarise(n=n()) %>% ungroup() %>% 
    mutate(stocknum=ceiling(n/indexINFO$compNUM[k]*indexINFO$selcNUM[k])) %>% select(-n)
  TSS <- TSS %>% left_join(re,by=c('date','sector')) %>% arrange(date,sector,stocknum)
  
  if(indexINFO$indexID[k]=='EI000300'){
    constr <- constr_default(box_each = c(0,0.1))
    constr <- addConstr_box(constr,each=c(-0.015,0.015),relative = 1)
  }else if(indexINFO$indexID[k]=='EI000905'){
    constr <- constr_default(box_each = c(0,0.05))
    constr <- addConstr_box(constr,each=c(-0.01,0.01),relative = 1)
  }
  
  #sector neutural
  constr_sec <- addConstr_fctExp_sector(constr,each = c(-0.05,0.05))
  
  #sector size neutural
  constr_size_sec <- addConstr_fctExp_style(constr_sec,FactorLists[2],min =-0.1,max=0.1)
  
  #size neutural
  constr_size <- addConstr_fctExp_style(constr,FactorLists[2],min =-0.1,max=0.1)
  
  for(i in 1:nport){
    cat(i,'...\n')
    TS_ <- TS %>% group_by(date) %>% sample_n(indexINFO$selcNUM[k])
    TS_ <- as.data.frame(TS_)
    
    for(j in 1:nrow(wgtTYPEDF)){
      cat('...',wgtTYPEDF$typeName[j],'...\n')
      if(wgtTYPEDF$typeName[j]=='简单随机'){
        
        if(indexINFO$indexID[k]=='EI000300'){
          max_wgt_ <- 0.1
        }else if(indexINFO$indexID[k]=='EI000905'){
          max_wgt_ <- 0.05
        }
        port <- addwgt2port(TS_,wgtType = wgtTYPEDF$wgtType[j],max_wgt = max_wgt_)
        
      }else if(wgtTYPEDF$typeName[j]=='简单随机等权'){
        port <- addwgt2port(TS_,wgtType = wgtTYPEDF$wgtType[j])
        
      }else if(wgtTYPEDF$typeName[j]=='简单随机市值中性'){
        port <- addwgt2port(TS_,wgtType = wgtTYPEDF$wgtType[j],sectorNe_wgt = defaultSectorAttr(type = 'fct'))
        
      }else if(wgtTYPEDF$typeName[j]=='简单随机行业中性'){
        TS_ <- TSS %>% group_by(date) %>% mutate(factorscore=rnorm(n())) %>% ungroup() %>% 
          arrange(date,sector,factorscore) %>% group_by(date,sector) %>% mutate(rownum=row_number()) %>% 
          filter(rownum<=stocknum) %>% ungroup() %>% select(date,stockID)
        TS_ <- as.data.frame(TS_)
        
        port <- addwgt2port(TS_,wgtType = wgtTYPEDF$wgtType[j],sectorNe_wgt = defaultSectorAttr(),wgtbmk = indexINFO$indexID[k])
      }else if(wgtTYPEDF$wgtType[j]=='optimize'){
        TSF <- TS %>% group_by(date) %>% mutate(factorscore=rnorm(n())) %>% ungroup()
        TSF <- as.data.frame(TSF)
        if(wgtTYPEDF$typeName[j]=='行业中性'){
          port <- getPort_opt(TSF,constr = constr_sec,bmk = indexINFO$indexID[k])
        }else if(wgtTYPEDF$typeName[j]=='行业市值中性'){
          port <- getPort_opt(TSF,constr = constr_size_sec,bmk = indexINFO$indexID[k])
        }else if(wgtTYPEDF$typeName[j]=='市值中性'){
          port <- getPort_opt(TSF,constr = constr_size,bmk = indexINFO$indexID[k])
        }
      }
      

      port <- port[,c("date","stockID","wgt")]
      re <- port.backtest(port,holdingEndT=endT)
      port <- port %>% mutate(type=wgtTYPEDF$typeName[j],fundID=paste(indexINFO$indexAbbr[k],wgtTYPEDF$typeAbbr[j],'_',i,sep = ''),
                              trackindexcode=indexINFO$indexID[k])
      totport <- rbind(totport,port)
      bench <- getrtn.bmk(re,indexINFO$indexID[k])
      re <- merge(re,bench)
      re <- xts2df(re)
      re <- re %>% mutate(type=wgtTYPEDF$typeName[j],fundID=paste(indexINFO$indexAbbr[k],wgtTYPEDF$typeAbbr[j],'_',i,sep = ''),
                          trackindexcode=indexINFO$indexID[k]) %>% 
        rename(nav_rtn=portfolioReturns,bmk_rtn=bmk) %>% select(type,date,fundID,nav_rtn,trackindexcode,bmk_rtn)
      randomnav <- rbind(randomnav,re)
  
    }
  }
}




fundtype <- unique(randomnav[,c("type","fundID","trackindexcode")])

randstat <- MF_nav_stat(fundnav = randomnav)
randstat <- randstat %>% select(-nyear,-rtn_min,-rtn_max,-rtn_mindate,-rtn_maxdate) %>%
  left_join(fundtype,by='fundID')
randstatyear <- MF_nav_stat(freq = 'year',fundnav = randomnav)
randstatyear <- randstatyear %>% select(-nyear,-rtn_ann,-bench_ann,-alpha_ann,-rtn_min,-rtn_max,-rtn_mindate,-rtn_maxdate) %>%
  left_join(fundtype,by='fundID')

########################****************#################################
########################~~ true port ~~######################################
########################****************#################################

indexINFO <- data.frame(indexID=c('EI000300','EI000905'),indexAbbr=c('hs300','zz500'),
                        compNUM=c(300,500),selcNUM=c(150,250),stringsAsFactors = FALSE)

wgtTYPEDF <- data.frame(typeName=c('简单随机行业中性','行业中性'),
                        typeAbbr=c('randsec','sec'),
                        wgtType=c('ffv','optimize'),
                        stringsAsFactors = FALSE)

begT <- as.Date('2010-01-04')
endT <- as.Date('2018-03-13')
RebDates <- getRebDates(begT,endT,'month')
RebDates <- RebDates[1:(length(RebDates)-1)]

#sector neutral
#multiple factors

conLists <- buildFactorLists(
  buildFactorList(factorFun="gf.ln_mkt_cap",
                  factorDir=1),
  factorRefine=refinePar_default("scale")
)
FactorLists <- conLists <- buildFactorLists(
  buildFactorList(factorFun="gf.SIZE"),
  buildFactorList(factorFun="gf.GROWTH"),
  buildFactorList(factorFun="gf.TRADING"),
  buildFactorList(factorFun="gf.EARNINGYIELD"),
  buildFactorList(factorFun="gf.VALUE"),
  buildFactorList(factorFun="gf.OTHER")
)
nfactor <- length(FactorLists)

randomnav <- data.frame()
totport <- data.frame()
for(k in 1:nrow(indexINFO)){
  cat(indexINFO$indexID[k],'...\n')
  TS <- getTS(RebDates,indexINFO$indexID[k])
  mTSF <- getMultiFactor(TS,FactorLists)
  
  mTSFR <- getTSR(mTSF)
  reg_results <- reg.TSFR(mTSFR)
  
  ##get factor return, factor covariance and stock's residual
  rtn_cov_delta <- f_rtn_cov_delta(reg_results=reg_results,rolling = TRUE,nwin = 12)
  fRtn <- rtn_cov_delta$fRtn
  fCov <- rtn_cov_delta$fCov
  delta <- rtn_cov_delta$Delta
  
  TSS <- getSectorID(TS,fillNA = TRUE)
  re <- TSS %>% group_by(date,sector) %>% summarise(n=n()) %>% ungroup() %>% 
    mutate(stocknum=ceiling(n/indexINFO$compNUM[k]*indexINFO$selcNUM[k])) %>% select(-n)
  TSS <- TSS %>% left_join(re,by=c('date','sector')) %>% arrange(date,sector,stocknum)
  TSSF <- MultiFactor2CombiFactor(mTSF,wgts = rep(1/nfactor,nfactor))
  TSSF <- TSSF %>% left_join(TSS,by=c('date','stockID')) %>% select(date,stockID,sector,factorscore,stocknum) %>% 
    filter(date>=min(fRtn$date),date<=max(fRtn$date))
  
  if(indexINFO$indexID[k]=='EI000300'){
    constr <- constr_default(box_each = c(0,0.1))
    constr <- addConstr_box(constr,each=c(-0.015,0.015),relative = 1)
  }else if(indexINFO$indexID[k]=='EI000905'){
    constr <- constr_default(box_each = c(0,0.05))
    constr <- addConstr_box(constr,each=c(-0.01,0.01),relative = 1)
  }
  constr <- addConstr_fctExp_sector(constr,each = c(-0.05,0.05))
  constr <- addConstr_trackingerror(constr,trackingerror_ann = 0.05)
  
  
  for(j in 1:nrow(wgtTYPEDF)){
    cat('...',wgtTYPEDF$typeName[j],'...\n')
    
    if(wgtTYPEDF$typeName[j]=='简单随机行业中性'){
      TS_ <- TSSF %>% arrange(date,sector,desc(factorscore)) %>% group_by(date,sector) %>% mutate(rownum=row_number()) %>% 
        filter(rownum<=stocknum) %>% ungroup() %>% select(date,stockID)
      TS_ <- as.data.frame(TS_)
      
      port <- addwgt2port(TS_,wgtType = wgtTYPEDF$wgtType[j],sectorNe_wgt = defaultSectorAttr(),wgtbmk = indexINFO$indexID[k])
    }else if(wgtTYPEDF$wgtType[j]=='optimize'){
      mTSF <- mTSF %>% filter(date>=min(fRtn$date),date<=max(fRtn$date))
      fRtn <- transform(fRtn,frtn=0.001)
      port <- getPort_opt(mTSF,fRtn = fRtn,fCov = fCov,delta = delta,constr = constr,bmk = indexINFO$indexID[k])

    }
    
    port <- port[,c("date","stockID","wgt")]
    re <- port.backtest(port,holdingEndT=endT)
    port <- port %>% mutate(type=wgtTYPEDF$typeName[j],fundID=paste(indexINFO$indexAbbr[k],wgtTYPEDF$typeAbbr[j],'_',sep = ''),
                            trackindexcode=indexINFO$indexID[k])
    totport <- rbind(totport,port)
    bench <- getrtn.bmk(re,indexINFO$indexID[k])
    re <- merge(re,bench)
    re <- xts2df(re)
    re <- re %>% mutate(type=wgtTYPEDF$typeName[j],fundID=paste(indexINFO$indexAbbr[k],wgtTYPEDF$typeAbbr[j],'_',sep = ''),
                        trackindexcode=indexINFO$indexID[k]) %>% 
      rename(nav_rtn=portfolioReturns,bmk_rtn=bmk) %>% select(type,date,fundID,nav_rtn,trackindexcode,bmk_rtn)
    randomnav <- rbind(randomnav,re)
  }
  
}


fundtype <- unique(randomnav[,c("type","fundID","trackindexcode")])

randstat <- MF_nav_stat(fundnav = randomnav)
randstat <- randstat %>% select(-nyear,-rtn_min,-rtn_max,-rtn_mindate,-rtn_maxdate) %>%
  left_join(fundtype,by='fundID')
randstatyear <- MF_nav_stat(freq = 'year',fundnav = randomnav)
randstatyear <- randstatyear %>% select(-nyear,-rtn_ann,-bench_ann,-alpha_ann,-rtn_min,-rtn_max,-rtn_mindate,-rtn_maxdate) %>%
  left_join(fundtype,by='fundID')

