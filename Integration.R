### 將 result.txt (框的資訊) 轉換成實際座標 

wd = 'G:/我的雲端硬碟/2021_Fall/Graduation thesis/20220321/'

# 讀入 result 的資訊，轉成 list
# bbox = read.delim(file.choose(),sep='\n')
bbox = read.delim(paste0(wd,'gc_d.txt'),sep='\n')

{
  stride = 1 # 生成影像的間隔
  vlength = 42 # 影片長度
  eps = 3 # dbscan 半徑
  minPts = 10 # dbscan 最小鄰居數
  start = Sys.time()
  {
    library(magrittr)
    all_scene_object = list()
    j=1
    
    for(i in 1:NROW(bbox)){
      if(grep('person',bbox[i,])%>%length()){ # if 'person' in the row
        tmp = strsplit(bbox[i,],' ') %>% unlist # split the row into piece of vector
        match = regmatches(tmp, gregexpr("[[:digit:]]+", tmp)) %>% unlist %>% as.numeric() # get confidence and coordinate
        match = c(match[1], tail(match,4)) # if the length of a vector > 5, then save first(person) and the last 4 (coordinate)  
        if(length(all_scene_object)<j)  all_scene_object[[j]] = NA # give NA first
        all_scene_object[[j]] = rbind(all_scene_object[[j]], match) # row bind
        
      }
      if(grep('AVG',bbox[i,])%>%length()){
        all_scene_object[[j]] = all_scene_object[[j]][-1,] # delete the first 'NA' row
        j = j + 1 # go to next
      }
    }
    A = all_scene_object 
    
    # adjust height of boxes 
    
    get_height = function(data){
      return(data[rowSums(data[,c(3,5)]) %in% 950:1079, 5] %>% as.vector())
    }
    height = lapply(A, get_height) %>% unlist()
    
    for(i in 1:length(A)){
      set.seed(i)
      n = A[[i]][rowSums(A[[i]][,c(3,5)])==1080, 5] %>% length()
      A[[i]][rowSums(A[[i]][,c(3,5)])==1080, 5] = runif(n, min = quantile(height)[3], max = quantile(height)[4])
    }
    # 用整體的 ratio 設門檻
    # get_ratio = function(data) return(data[,4]/data[,5])
    # ratio = lapply(A, get_ratio) %>% unlist()
    # threshold = quantile(ratio[ratio < 1], probs = 0.995)
    # for(i in 1:length(A)){
    #   set.seed(i)
    #   wh_ratio = A[[i]][,4]/A[[i]][,5]
    #   n = A[[i]][wh_ratio > threshold, 5] %>% length()
    #   ratio_range = runif(n, min = quantile(ratio[ratio < 1])[2], max = quantile(ratio[ratio < 1])[4])
    #   A[[i]][wh_ratio > threshold, 5] = A[[i]][wh_ratio > threshold, 4]/ratio_range
    # }
  }
  {
    # combine the targets into series
    series = list()
    series[[1]] = A[[1]]
    
    # index is for the matching(next scene)
    index = 2
    # the next scene
    A_next = A[[index]]
    # threshold for bounding box matching, if < threshold, then matching
    threshold = 60
    # latest scene of objects
    latest = rep(1, NROW(A[[1]]))
    # select objects within 30 frames
    frame.interval = 30
    # record MAEs
    MAE.vec = c()
  }
  
  
  # run all scenes
  for(r in 1:(length(A)-1)){
    series[[r+1]] = array(NA, dim(series[[r]]))
    not_matched = c()
    match_check = matrix(NA, length(latest), 2)
    match_type = matrix(0, NROW(A_next), 2)
    match_latest = which(is.na(match_check[,1]))
    while(!all(match_type[,2]!=0)){
      
      for(i in which((match_type[,1] == 0) & (match_type[,2] != -1))){
        
        benchmark = 10**4
        match = 1
        for(j in intersect(which((index-latest) < frame.interval), match_latest)){
          MAE = mean(abs(series[[latest[j]]][j,] - A_next[i,])*c(0.5,1,1,1,0.5))
          if(MAE < benchmark){
            benchmark = MAE
            match = j # match nearest neighbor
            MAE.vec = c(MAE.vec, MAE)
          }            
        }
        
        if(benchmark <= threshold){
          
          if(!is.na(series[[index]][match,][1])){
            
            if(match_check[match,2] >= benchmark){
              
              series[[index]][match,] = A_next[i,] # update
              match_type[match_check[match,1],]=c(0,0) # next 暫時沒有配對: [被替換掉]
              match_check[match,] = c(i, benchmark) # update match_check
              match_type[i,]=c(1,1) # next 暫時有配對: (有配對, 有鄰居) [後來的]
              
            }
            if(match_check[match,2] < benchmark){
              match_type[i,]=c(0,0) # next 暫時沒有配對: [後來的比之前的誤差來的大]
            }
          }else{ # 第一次配對
            series[[index]][match,] = A_next[i,]
            match_check[match,] = c(i, benchmark)
            match_type[i,]=c(1,1) # next 暫時有配對: (有配對, 有鄰居)
          }
        }else{
          match_type[i,]=c(0,-1) # next 確定沒有配對: (沒有配對, 沒有鄰居)
        }           
      }
      match_latest = which(is.na(match_check[,1]))
    }
    not_matched = which(match_type[,2] == -1)
    series[[index]] = rbind(series[[index]],A_next[not_matched,])
    latest[which(!is.na(match_check[,1]))] = index
    latest = c(latest,rep(index,length(not_matched)))
    for(ind in 1:(index-1)){
      series[[ind]] = rbind(series[[ind]],array(NA,c(length(not_matched),5)))
    }
    if(index < length(all_scene_object)) index = index + 1
    benchmark = 10**4
    A_next = A[[index]]
    A_next = A_next[A_next[,4] < 200,] # 留 width < 200
  }
  
  ### Concatenate scene to a big matrix!
  {
    a=series[[1]]
    for(i in 2:length(A)){a=cbind(a,series[[i]])}
    # delete objects that appeared less than 4 scenes in total
    a = a[rowSums(!is.na(a))>15, ]
    
  }
  ### 補中間缺失 
  {
    fillcount=0
    frame.interval.2 = 8 # about 5 frames
    start = Sys.time()
    begin=1
    anchor1=NA
    anchor2=NA
    for(i in 1:NROW(a)){
      check_point=0
      for(j in 1:(NCOL(a)/5)){
        if(!is.na(a[i,begin]) & (begin+4)/5 <= j){
          anchor2=5*j
          if(is.na(a[i, anchor2])) check_point=1
          if(!is.na(a[i,5*j]) & check_point == 0) anchor1=5*j
          if((!is.na(a[i, anchor1])) & (!is.na(a[i, anchor2])) & (anchor1 != anchor2)){
            p = a[i,(anchor1-3):anchor1] # 取 x,y,w,h
            q = a[i,(anchor2-3):anchor2]
            if((anchor2-anchor1) < frame.interval.2*5){
              fillcount = fillcount+1
              for(k in 2:5){
                a[i,seq(anchor1+k, anchor2-10+k, 5)] = seq(p[k-1], q[k-1],
                                                           length.out = (anchor2-anchor1)/5+1)[-c(1,(anchor2-anchor1)/5+1)]
              }
            }
            anchor1 = anchor2
            check_point = 0
          }
        }else if((begin+4)/5 <= j){begin = begin + 5}
      }
      anchor1 = NA
      anchor2 = NA
    }
    end = Sys.time()
    cat('Total Time:', end - start)

  }

  ### coordinate transformation (perspective mappings)
  {
    matched_data = a[,(1:NCOL(a))[1:NCOL(a) %% 5 !=1]]
    real_location = function(x,y,w,h){
      A=cbind(x+w/2, y+h) 
      return(A)
    }
    real_location_data = function(data){
      output = 1
      for(i in 0:(NCOL(data)/4-1)){
        A = data[,(4*i+1):(4*i+4)]
        Ax = A[,1]
        Ay = A[,2]
        Aw = A[,3]
        Ah = A[,4]
        if(output==1){
          output = real_location(Ax, Ay, Aw, Ah)
        }else{output = cbind(output, real_location(Ax, Ay, Aw, Ah))}
      }
      return(output)
    }
    rl_data = real_location_data(matched_data)
    for(i in 0:(NCOL(rl_data)/2-1)){
      rl_data[,c(2*i+1,2*i+2)]=cbind(rl_data[,c(2*i+2,2*i+1)])
    }
  }  
    
  {
    options(digits=10)
    quad_trans = function(p00, p10, p01, p11){
      Ap=matrix(c(p10-p00,0,p01-p00,0,p00,1),3,3)
      return(list(x=(solve(Ap) %*% c(p11,1))[1:2], Ap2=Ap[1:2,1:2], Ap2.inv=solve(Ap[1:2,1:2]), Ap=Ap))
    }
    
    F_transformation = function(p, p00, p10, p01, p11, q00, q10, q01, q11){
      a=quad_trans(p00, p10, p01, p11)[[1]][1]
      b=quad_trans(p00, p10, p01, p11)[[1]][2]
      c=quad_trans(q00, q10, q01, q11)[[1]][1]
      d=quad_trans(q00, q10, q01, q11)[[1]][2]
      s=a+b-1
      t=c+d-1
      Ap=quad_trans(p00, p10, p01, p11)[[4]]
      p_new=(solve(Ap) %*% c(p,1))[1:2]
      denominator = (b*(c*s-a*t)*p_new[1]+a*(d*s-b*t)*p_new[2]+a*b*t)
      f = c(b*c*s*p_new[1],a*d*s*p_new[2])/denominator
      Aq = quad_trans(q00, q10, q01, q11)[[4]]
      out = (Aq %*% c(f,1))[1:2]
      return(out)
    }
    pixels_to_coordinates = function(p){
      
      # p00=c(0, 0)
      # p10=c(13.8, 0)
      # p01=c(0, 14.625)
      # p11=c(13.8, 14.625)
      p00=c(0, 13.8)+c(1,1)
      p10=c(0, 0)+c(1,1)
      p01=c(14.625, 13.8)+c(1,1)
      p11=c(14.625, 0)+c(1,1)
      q00=c(0, 0)
      q10=c(410, 0)
      q01=c(0, 615)
      q11=c(410, 790)
      
      # oxford
      # p00=c(10, 10)
      # p10=c(10, 23)
      # p01=c(33, 23)
      # p11=c(33, 10)
      # q00=c(580,-200)
      # q10=c(900,1320)
      # q01=c(190,1920)
      # q11=c(100,1000)
      
      # for result1
      # p00=c(34.66875673883649, 135.50122202744126)
      # p10=c(34.668739050820385, 135.5012971016943)
      # p01=c(34.66882805620475, 135.50122300341607)
      # p11=c(34.668825298714076, 135.50137991263927)
      # q00=c(1070,410)
      # q10=c(940,1800)
      # q01=c(715,245)
      # q11=c(580,1440)
      
      # for result2
      # p00=c(34.66875673883649, 135.50122202744126)
      # p10=c(34.66874932618074, 135.5012960987769)
      # p01=c(34.66882805620475, 135.50122300341607)
      # p11=c(34.668825298714076, 135.50137991263927)
      # q00=c(1040,590)
      # q10=c(985,1890)
      # q01=c(700,400)
      # q11=c(575,1610)
      return(F_transformation(p, q00, q10, q01, q11, p00, p10, p01, p11))
      
    }
  }  
    
  {
    rl_data_coor = matrix(NA, NROW(rl_data), NCOL(rl_data))
    for(i in 0:(NCOL(rl_data)/2-1)){
      for(j in (1:NROW(rl_data))){
        rl_data_coor[j,c(2*i+1,2*i+2)] = pixels_to_coordinates(rl_data[j,c(2*i+1,2*i+2)])
      }
    }
    
    coordinates_to_meter_coordinates = function(p){
      
      b00=c(0, 13.8)+c(1,1)
      b10=c(0, 0)+c(1,1)
      b01=c(14.625, 13.8)+c(1,1)
      b11=c(14.625, 0)+c(1,1)
      c00=c(0, 13.8)+c(1,1)
      c10=c(0, 0)+c(1,1)
      c01=c(14.625, 13.8)+c(1,1)
      c11=c(14.625, 0)+c(1,1)
      # b00=c(0, 0)
      # b10=c(13.8, 0)
      # b01=c(0, 14.625)
      # b11=c(13.8, 14.625)
      # c00=c(0, 0)
      # c10=c(13.8, 0)
      # c01=c(0, 14.625)
      # c11=c(13.8, 14.625)
      
      # b00=c(10, 10)
      # b10=c(10, 23)
      # b01=c(33, 23)
      # b11=c(33, 10)
      # c00=c(10, 10)
      # c10=c(10, 23)
      # c01=c(33, 23)
      # c11=c(33, 10)
      
      # b00=c(34.6687, 135.5012)
      # b10=c(34.6687, 135.5014)
      # b01=c(34.6689, 135.5012)
      # b11=c(34.6689, 135.5014)
      # c00=c(0,0)
      # c10=c(18.331,0)
      # c01=c(0,22.187)
      # c11=c(18.331,22.187)
      return(F_transformation(p, b00, b10, b01, b11, c00, c10, c01, c11))
      
    }
    meter_rl_data_coor = matrix(NA, NROW(rl_data_coor), NCOL(rl_data_coor))
    for(i in 0:(NCOL(rl_data)/2-1)){
      for(j in (1:NROW(rl_data_coor))){
        meter_rl_data_coor[j,c(2*i+1,2*i+2)] = coordinates_to_meter_coordinates(rl_data_coor[j,c(2*i+1,2*i+2)])
      }
    }
  }
  end = Sys.time()
  cat('Total Time:', end - start)
}

# {
#   max_glr = function(data, r){
#     dist_mat = as.matrix(dist(data, diag = T, upper = T))
#     # count number of neighbors given a radius
#     
#     c_A=c()
#     for(i in 1:NROW(data)) c_A[i] = sum(dist_mat[i,] < r)
#     # mu_A=mean(c_A)
#     mu_A = NROW(data)*(r^2*pi)/(20*24)
#     true_glr = ((c_A/mu_A)^c_A)*((NROW(data)-c_A)/(NROW(data)-mu_A))^(NROW(data)-c_A)
#     
#     return(true_glr)
#   }
#   
#   # implementation of Monte Carlo hypothesis testing
#   
#   mcht = function(data0, r, perm){
#     glr_sim = c() # simulate glr
#     for(seed in 1:perm){
#       set.seed(seed)
#       # randomly combine X and Y to simulate data
#       X_ran = runif(NROW(data0), min(data0[,1]), max(data0[,1]))
#       Y_ran = runif(NROW(data0), min(data0[,2]), max(data0[,2]))
#       
#       data_sim = cbind(X_ran, Y_ran)
#       
#       glr_sim[seed] = max(max_glr(data_sim, r))
#       
#     }
#     
#     return(glr_sim)
#   }
#   
#   ### Simplify to a function
#   
#   SSS = function(data, r, perm, alpha){
#     row.index = rownames(data)
#     true_glr_1 = max_glr(data, r)
#     glr_sim_1 = mcht(data, r, perm)
#     if((1 - sum(max(true_glr_1) > glr_sim_1)/(perm + 1)) <= alpha){
#       # if it has cluster, then return cluster index
#       mlc.check = 1
#       true_glr_1.dec = sort(true_glr_1, decreasing = T)
#       ind = 1
#       while(mlc.check){
#         if((1 - sum(true_glr_1.dec[ind+1] > glr_sim_1)/(perm + 1)) <= alpha){
#           ind = ind + 1
#         }else{mlc.check = 0}
#       }
#       
#       return(list(index = row.index[which(true_glr_1 %in% true_glr_1.dec[1:ind])],
#                   cluster = 1))
#     }else{return(list(index = 0, cluster = 0))}
#   }
# }

# Density statistics
{
  max_density = function(data, r, beta){
    dist_mat = as.matrix(dist(data, diag = T, upper = T))
    num_of_nbd = apply(dist_mat, 2, function(x) sum(x <= r))
    mean_of_dist = apply(dist_mat, 2, function(x) mean(x[x <= r]))
    return(sapply(beta, function(x) (num_of_nbd - x*mean_of_dist)*(num_of_nbd > 2))) 
    # matrix, row: number of data, col: beta
  }
  
  # implementation of Monte Carlo hypothesis testing
  
  mcht_density = function(data, r, beta, perm){
    den_sim = matrix(NA, length(beta), perm) # simulate density
    for(seed in 1:perm){
      set.seed(seed)
      # randomly combine X and Y to simulate data
      X_ran = runif(NROW(data), min(data[,1]), max(data[,1]))
      Y_ran = runif(NROW(data), min(data[,2]), max(data[,2]))
      data_sim = cbind(X_ran, Y_ran)
      den_sim[,seed] = apply(max_density(data_sim, r, beta), 2, max)
    }
    den_sim = t(den_sim)
    return(den_sim) # matrix, row: perm, column: beta
  }
  
  ### Simplify to a function
  
  DS = function(data, r, beta, perm, alpha){
    row.index = rownames(data)
    DS_list = list()
    true_max_density = max_density(data, r, beta)
    max_density_sim = mcht_density(data, r, beta, perm)
    # cat('max(true_max_density) = ',max(true_max_density))
    # print(max_density_sim)
    for(b in 1:length(beta)){
      d.ratio = sum(max(true_max_density[,b]) > max_density_sim[,b])/(perm + 1)
      if((1 - d.ratio) <= alpha){
        # if it has cluster, then return cluster index
        mlc.check = 1
        true_max_density.dec = sort(true_max_density[,b], decreasing = T)
        ind = 1
        while(mlc.check){
          if((1 - sum(true_max_density.dec[ind+1] > max_density_sim[,b])/(perm + 1)) <= alpha){
            ind = ind + 1
          }else{mlc.check = 0}
        }
        DS_list[[b]] = list(beta = beta[b], 
                            index = row.index[which(true_max_density[,b] %in% true_max_density.dec[1:ind])],
                            cluster = 1,
                            max.density = max(true_max_density[,b]),
                            d.ratio = d.ratio)
        
      }else{
          DS_list[[b]] = list(beta = beta[b], 
                              index = 0, 
                              cluster = 0, 
                              max.density = max(true_max_density[,b]),
                              d.ratio = d.ratio)
      }
    }
    return(DS_list)
  }
}

{
  library(spatstat) 
  library(sp)
  library(dbscan)
  
  max.K.loc = function(data, r){
    dist_mat = as.matrix(dist(data, diag = T, upper = T))
    num_of_nbd = apply(dist_mat, 2, function(x) sum(x <= r))
    return(rownames(dist_mat)[which.max(num_of_nbd)])
  }
}

{
  start = Sys.time()
  beta.vec = seq(5.2,10,0.2)
  DS_beta_cluster_density = matrix(NA, NCOL(meter_rl_data_coor)/2, length(beta.vec))
  DS_beta_cluster_density2 = matrix(NA, NCOL(meter_rl_data_coor)/2, length(beta.vec))
  DS_beta_cluster_ratio = matrix(NA, NCOL(meter_rl_data_coor)/2, length(beta.vec))
  mlc = matrix(0, nrow = NROW(rl_data), ncol = NCOL(rl_data)/2)
  # DS_beta_cluster = matrix(NA, NCOL(meter_rl_data_coor)/2, length(beta.vec))
  
  rownames(meter_rl_data_coor) = 1:NROW(meter_rl_data_coor)
  
  for(i in 0:(NCOL(meter_rl_data_coor)/2-1)){
    
    if((i %% 10 == 0)){
      print(paste('Processing --- ', signif(i/(NCOL(meter_rl_data_coor)/2)*100, 2),' % ---'))
    }
    
    data=na.omit(meter_rl_data_coor[,c(2*i+1,2*i+2)])
    
    test = DS(data = data, r = 3, beta = beta.vec, perm = 499, alpha = 0.05)
    DS_beta_cluster_density[(i+1),] = unlist(lapply(test, function(x) x$max.density))
    DS_beta_cluster_ratio[(i+1),] = unlist(lapply(test, function(x) x$d.ratio))
    DS_beta_cluster_density2[(i+1),] = unlist(lapply(test, function(x) x$max.density))
    mlc[as.integer(test$index), i+1] = 1
    # DS_beta_cluster[(i+1),] = unlist(lapply(test, function(x) x$index))
  }
  
  end = Sys.time()
  cat('Total Time:', end - start)
}

{
  max.K.r = c()
  mKl = c()
  K.diff = c()
  mean.cl.K = c()
  max.density.DS = c()
  num.cluster = c()
  qd22.c = c()
  qd22.m = c()
  qd22G = c()
  qd22F = c()
  qd22N = c()
  qd22Gm = c()
  qd3.c = c()
  qd3.m = c()
  qd3G = c()
  qd3F = c()
  qd3N = c()
  qd3Gm = c()
  qd4.c = c()
  qd4.m = c()
  qd4G = c()
  qd4F = c()
  qd4N = c()
  qd4Gm = c()
}
{
  # window for study
  W = owin(c(0, 17),
           c(0, 17))
  # W = owin(c(0, 20),
  #          c(0, 24))
  # W = owin(c(0, 40),
  #          c(0, 25))
  detection.method = c(1)
  start = Sys.time()
  mlc = matrix(0, nrow = NROW(rl_data), ncol = NCOL(rl_data)/2)
  # cluster_scenes_result = matrix(NA, NCOL(meter_rl_data_coor)/2, length(detection.method), 
  #                                dimnames = list(row=c(), (c('K', 'DS', 'DBSCAN'))[detection.method]))
  
  rownames(meter_rl_data_coor) = 1:NROW(meter_rl_data_coor)
  # Ripley's K function
  KK = function(x, r) Kest(X = x, r = seq(0, 4, 0.05), correction="best")
  
  for(i in 0:(NCOL(meter_rl_data_coor)/2-1)){
    
    if((i %% 10 == 0)){
      print(paste('Processing --- ', signif(i/(NCOL(meter_rl_data_coor)/2)*100, 2),' % ---'))
    }
    
    data=na.omit(meter_rl_data_coor[,c(2*i+1,2*i+2)])
    
    # ### K function
    if(1 %in% detection.method){
      pp1 = as.ppp(data, W = W)
      set.seed(i)
      test_K = spatstat.core::envelope(pp1, fun= KK, nsim = 99, nrank = 1, verbose=F)
      r = test_K$r[which.max(test_K$obs - test_K$hi)]
      test = ifelse(test_K$obs > test_K$hi,1,0)
      cluster.result.K = ifelse(mean(test) > 0.3,1,0)
      
      # cluster_scenes_result[i+1, 'K'] = cluster.result.K
      mKl[i+1] = as.integer(max.K.loc(data, r))*cluster.result.K
      # K.diff[i+1] = max(test_K$obs - test_K$hi)*cluster.result.K
      mean.cl.K[i+1] = mean(test[20:length(test)])
    }
    
    if(2 %in% detection.method){
      test = DS(data = data, r = 2, beta = 1.2, perm = 499, alpha = 0.05)
      # cluster_scenes_result[i+1, 'DS'] = test$cluster
      mlc[as.integer(test$index), i+1] = 1
      max.density.DS[i+1] = test$max.density
    }
    
    if(3 %in% detection.method){
      test = dbscan(x = data, eps = 2, minPts = 5)
      # cluster_scenes_result[i+1, 'DBSCAN'] = max(test$cluster)
      num.cluster[i+1] = sum(test$cluster != 0)
    }
    
    if(4 %in% detection.method){
      test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 2, ny = 2), method="Chisq")
      qd22.c[i+1] = 1 - test$p.value
      test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 2, ny = 2), method="MonteCarlo", nsim=499)
      qd22.m[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 2, ny = 2), CR = 0)
      # qd22G[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 2, ny = 2), CR = -1/2)
      # qd22F[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 2, ny = 2), CR = -1)
      # qd22N[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 2, ny = 2), CR = -2)
      # qd22Gm[i+1] = 1 - test$p.value
      
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 3, ny = 3), method="Chisq")
      # qd3.c[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 3, ny = 3), method="MonteCarlo", nsim=499)
      # qd3.m[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 3, ny = 3), CR = 0)
      # qd3G[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 3, ny = 3), CR = -1/2)
      # qd3F[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 3, ny = 3), CR = -1)
      # qd3N[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 3, ny = 3), CR = -2)
      # qd3Gm[i+1] = 1 - test$p.value
      # 
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 4, ny = 4), method="Chisq")
      # qd4.c[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 4, ny = 4), method="MonteCarlo", nsim=499)
      # qd4.m[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 4, ny = 4), CR = 0)
      # qd4G[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 4, ny = 4), CR = -1/2)
      # qd4F[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 4, ny = 4), CR = -1)
      # qd4N[i+1] = 1 - test$p.value
      # test = quadrat.test(X = quadratcount(as.ppp(data, W = W),nx = 4, ny = 4), CR = -2)
      # qd4Gm[i+1] = 1 - test$p.value
    }
    
  }
  
  end = Sys.time()
  cat('Total Time:', end - start)
}

### choose features
train.features = cbind(DS_beta_cluster_density, mean.cl.K,
                       qd22.c, qd22.m)
train.features = cbind(DS_beta_cluster_ratio, mean.cl.K,
                       qd22.c, qd22.m)

# train.features = cbind(DS_beta_cluster_density, mKl, mean.cl.K, K.diff, num.cluster,
#                        qd22.c, qd22.m, qd22G, qd22F, qd22N, qd22Gm,
#                        qd3.c, qd3.m, qd3G, qd3F, qd3N, qd3Gm,
#                        qd4.c, qd4.m, qd4G, qd4F, qd4N, qd4Gm)
# train.features = cbind(DS_beta_cluster_ratio, mKl, mean.cl.K, K.diff, num.cluster,
#                        qd22.c, qd22.m, qd22G, qd22F, qd22N, qd22Gm,
#                        qd3.c, qd3.m, qd3G, qd3F, qd3N, qd3Gm,
#                        qd4.c, qd4.m, qd4G, qd4F, qd4N, qd4Gm)

test.features = cbind(DS_beta_cluster_density, mKl, mean.cl.K, K.diff, num.cluster, qd22, qd33)
test.features = cbind(DS_beta_cluster_ratio, mKl, mean.cl.K, K.diff, num.cluster, qd22, qd33)

### For prediction of classification model

# install.packages('kmed')
library(kmed)

features = cbind(DS_beta_cluster_ratio_raw[,1],train.features[,26:28])
features = train.features
DS_beta_cluster_ratio_raw = features[,1:25]
DS_beta_cluster_ratio_combined = cbind(DS_beta_cluster_ratio_raw,DS_beta_cluster_ratio)

# label with k-medoids or handmade
kmed.result = fastkmed(dist(features),2)
table(kmed.result$cluster, gclabel)
which((kmed.result$cluster == 1)&(gclabel == 1))
table(kmed.result$cluster, label)

osaka.kmedlabel = kmed.result$cluster
tc.kmedlabel = kmed.result$cluster
table(osaka.label, osaka.kmedlabel)
table(tc.label, tc.kmedlabel)

gclabel = c(rep(1,634),rep(0,632))
label = gclabel
label = kmed.result$cluster
table(gclabel,kmed.result$cluster)


tc.label = c(rep(1,661),rep(0,390))
tc.label[250:315]=0
label = tc.kmedlabel
features = feature.tc.r



is.na(train.features[,26]) %>% sum()

# 結論: 剩下變數跟全變數的預測能力幾乎一致
library(boot)
library(caret)
# library(doParallel)
library(magrittr)
all.data = cbind(features, label) %>% as.data.frame()

cl <- makePSOCKcluster(detectCores() - 1) # I'm using 3 cores.
registerDoParallel(cl)
features = feature.osaka
features = feature.tc

features = train.features[,1:28]
features = train.features[,c(1:25,27,30:31)]

features = test.features
# features = test.features[,-c(26,28,30)]
test = cbind(features, rep(1,NROW(features))) %>% as.data.frame()
test[,NCOL(test)] = test[,NCOL(test)] %>% as.factor()

all.data$label = as.factor(all.data$label)
method.list = c('glm', 'ctree', 'lda', 'knn')
pred.f1 = matrix(NA,NCOL(DS_beta_cluster_density), length(method.list))
pred.pre = matrix(NA,NCOL(DS_beta_cluster_density), length(method.list))
pred.rec = matrix(NA,NCOL(DS_beta_cluster_density), length(method.list))

all.data = cbind(features, label) %>% as.data.frame()
{
  start = Sys.time()
  pre = c()
  rec = c()
  
  for(m in 1:length(method.list)){
    for(j in 1:NCOL(DS_beta_cluster_density)){
      for(i in 1:10){
        feature.input = c(j,(NCOL(features)+1):NCOL(all.data))
        set.seed(i)
        # train = sample(1:NROW(all.data), 0.8*NROW(all.data))
        train.data = all.data[train,]
        test.data = all.data[-train,]
        
        trcl = trainControl(method="cv",
                            savePredictions=TRUE,
                            classProbs=TRUE,
                            number=5,
                            p=0.8,
                            summaryFunction = twoClassSummary,
                            returnResamp = "all",
                            allowParallel = TRUE)
        model <- train(
          make.names(label) ~ ., # make.names is to retain the name of label
          train.data[,feature.input], # <- new!
          method = method.list[m], 
          metric = "ROC",
          trControl = trcl, 
          na.action = na.pass
        )
        
        pred.result = predict(model, test.data[,feature.input])
        pre[i] = (table(test.data$label, pred.result)[2,2] )/sum(table(test.data$label, pred.result)[,2])
        rec[i] = (table(test.data$label, pred.result)[2,2] )/sum(table(test.data$label, pred.result)[2,])
        
      }
      f1 = 2*(pre*rec)/(pre+rec)
      pred.f1[j,m] = mean(f1)
      pred.pre[j,m] = mean(pre)
      pred.rec[j,m] = mean(rec)
    }
  }
  
  
  end = Sys.time()
  cat('Total Time:', end - start)
  # cat('Accuracy : ',mean(acc),'\n')
  # cat('Sensitivity : ',mean(sen),'\n')
  # cat('F1 score : ', mean(f1))
}

f1.gclabel.r2 = pred.f1
f1.gclabel.r = pred.f1
f1.gclabel.r12 = rbind(f1.gclabel.r, f1.gclabel.r2)
f1.kmed.r = pred.f1
sum(pred.raw.all != pred.f1)
pred.raw = pred.f1
f1.tc.kmed = pred.f1
f1.tc.label = pred.f1
f1.o.kmed = pred.f1
f1.o.label = pred.f1
pred.raw.all = pred.raw

pred.f1 = f1.tc.label

f1.gc.label = pred.f1
f1.gc.kmed = pred.f1
f1.gc.kmed.r = pred.f1

library(ggplot2)
library(reshape2)
{
  row.names(pred.f1) = seq(0.2,5,0.2)
  colnames(pred.f1) = method.list
  df = melt(pred.f1)
  colnames(df)[1:3] = c('beta','classifier','F1_score')
  # plot
  ggm = ggplot(data = df, aes(x=beta, y=F1_score)) + geom_line(aes(colour=classifier),size=2)
  ggm + theme(text = element_text(size = 20))  +
    ylim(0.7, 1)    
}

{
  row.names(f1.gclabel.r12) = seq(0.2,10,0.2)
  colnames(f1.gclabel.r12) = method.list
  df = melt(f1.gclabel.r12)
  colnames(df)[1:3] = c('beta','classifier','F1_score')
  # plot
  ggm = ggplot(data = df, aes(x=beta, y=F1_score)) + geom_line(aes(colour=classifier),size=2)
  ggm + theme(text = element_text(size = 20))  +
    ylim(0.3, 1)    
}
### for each feature
features = train.features[,c(1:28)]
all.data = cbind(features, label) %>% as.data.frame()
all.data$label = as.factor(all.data$label)
{
  start = Sys.time()
  pre = c()
  rec = c()
  
  for(m in 1:length(method.list)){
    
    for(i in 1:10){
      set.seed(i)
      train = sample(1:NROW(all.data), 0.8*NROW(all.data))
      train.data = all.data[train,]
      test.data = all.data[-train,]
      
      trcl = trainControl(method="cv", 
                          savePredictions=TRUE, 
                          classProbs=TRUE, 
                          number=5, 
                          p=0.8, 
                          summaryFunction = twoClassSummary, 
                          returnResamp = "all",
                          allowParallel = TRUE)
      model <- train(
        make.names(label) ~ ., # make.names is to retain the name of label
        train.data, # <- new!
        method = method.list[m], 
        metric = "ROC",
        trControl = trcl, 
        na.action = na.pass
      )
      
      pred.result = predict(model, test.data)
      pre[i] = (table(test.data$label, pred.result)[2,2] )/sum(table(test.data$label, pred.result)[,2])
      rec[i] = (table(test.data$label, pred.result)[2,2] )/sum(table(test.data$label, pred.result)[2,])
      
    }
    f1 = 2*(pre*rec)/(pre+rec)
    cat('method = ', method.list[m], '\n')
    cat('f1',mean(f1), '\n')
  }
  
  end = Sys.time()
  cat('Total Time:', end - start, '\n')
  cat('f1',mean(f1))
}
# (0.8723702 + 0.8923249 + 0.8577636 + 0.8784785)/4
# (0.767946 + 0.7872872 + 0.7428312 + 0.7859059)/4
stopCluster(cl)

f1.gc.label.K = mean(f1)
f1.gc.label.Q = mean(f1)

### prediction for label
label = gclabel
set.seed(1)
train = sample(1:NROW(all.data), 0.8*NROW(all.data))
label = kmed.result$cluster
features = train.features[,c(9:11,26:28)]
all.data = cbind(features, label) %>% as.data.frame()
all.data$label = as.factor(all.data$label)

train.data = all.data[train,]
test.data = all.data[-train,]

# trcl = trainControl(method="cv", 
#                     savePredictions=TRUE, 
#                     classProbs=TRUE, 
#                     number=5, 
#                     p=0.8, 
#                     summaryFunction = twoClassSummary, 
#                     returnResamp = "all",
#                     allowParallel = TRUE)
model <- train(
  make.names(label) ~ ., # make.names is to retain the name of label
  train.data, # <- new!
  method = "knn", 
  metric = "ROC",
  trControl = trcl, 
  na.action = na.pass
)

pred.result = predict(model, test.data)


table(test.data$label, pred.result)
A
table.kmed.K = table(test.data$label, pred.result)
table.kmed.Q = table(test.data$label, pred.result)
table.kmed.KQ = table(test.data$label, pred.result)
table.kmed.all = table(test.data$label, pred.result) # beta=0.2
table.kmed.all.r = table(test.data$label, pred.result) #5

table.gclabel.K = table(test.data$label, pred.result)
table.gclabel.Q = table(test.data$label, pred.result)
table.gclabel.KQ = table(test.data$label, pred.result)
table.gclabel.all = table(test.data$label, pred.result) # beta=0.2
table.gclabel.all.r = table(test.data$label, pred.result) #5
table.gclabel.all.r2 = table(test.data$label, pred.result)#7.2
f1_f = function(TABLE){
  pre_ = TABLE[2,2]/sum(TABLE[,2])
  rec_ = TABLE[2,2]/sum(TABLE[2,])
  return(2*(pre_*rec_)/(pre_+rec_))
}
f1_f(table.kmed.K)
f1_f(table.kmed.Q)
f1_f(table.kmed.KQ)
f1_f(table.kmed.all)
f1_f(table.kmed.all.r)
f1_f(table.gclabel.K)
f1_f(table.gclabel.Q)
f1_f(table.gclabel.KQ)
f1_f(table.gclabel.all)
f1_f(table.gclabel.all.r)
f1_f(table.gclabel.all.r2)

f1_dat = data.frame(F1_score = c(f1_f(table.kmed.K),
                                 f1_f(table.kmed.Q),
                                 f1_f(table.kmed.KQ),
                                 f1_f(table.kmed.all),
                                 f1_f(table.kmed.all.r),
                                 f1_f(table.gclabel.K),
                                 f1_f(table.gclabel.Q),
                                 f1_f(table.gclabel.KQ),
                                 f1_f(table.gclabel.all),
                                 f1_f(table.gclabel.all.r2)),
                    Label_type = c(rep('K-medoids',5),rep('label',5)),
                    feature = factor(rep(c('K','Q','K+Q','K+Q+DSS','K+Q+DSS_R'),2),
                                     levels = c('K','Q','K+Q','K+Q+DSS','K+Q+DSS_R')))
# levels(f1_dat$feature) = c('K','Q','K+Q','K+Q+S')
(f1_dat$feature) = as.factor(f1_dat$feature)
ggplot(data=f1_dat, aes(x=feature, y=F1_score, fill=Label_type)) +
  geom_bar(stat="identity", position=position_dodge()) +
  coord_cartesian(ylim = c(0.7, 1)) + theme(text = element_text(size = 20))

###
### compute the Most Likely Cluster matrix
###

mlc = matrix(0, nrow = NROW(rl_data), ncol = NCOL(rl_data)/2)
# DS_beta_cluster = matrix(NA, NCOL(meter_rl_data_coor)/2, length(beta.vec))

rownames(meter_rl_data_coor) = 1:NROW(meter_rl_data_coor)

for(i in 0:(NCOL(meter_rl_data_coor)/2-1)){
# for(i in 0:(100)){
  
  if((i %% 10 == 0)){
    print(paste('Processing --- ', signif(i/(NCOL(meter_rl_data_coor)/2)*100, 2),' % ---'))
  }
  
  data=na.omit(meter_rl_data_coor[,c(2*i+1,2*i+2)])
  
  test = DS(data = data, r = 3, beta = 0.2, perm = 499, alpha = 0.1)
  # DS_beta_cluster_density[(i+1),] = unlist(lapply(test, function(x) x$max.density))
  # DS_beta_cluster_ratio[(i+1),] = unlist(lapply(test, function(x) x$d.ratio))
  mlc[as.integer(test[[1]]$index), i+1] = 1
}  
  
  
{
  mlc.mat = mlc
  ac.mlc = function(x){
    for (i in 2:length(x)) {
      if(x[i] != 0) x[i] = x[i-1] + x[i]
    }
    return(x)
  }
  mlc.mat = t(apply(mlc.mat, 1, ac.mlc))
}


#####

# moving cluster

#####

library(rlist)
library(dbscan)

minPts
eps
{
  st.time = Sys.time()
  cl_mat = matrix(0, NROW(a), length(A)) 
  
  for(i in 1:length(A)){
    d = dbscan(na.omit(meter_rl_data_coor[,c(2*i-1, 2*i)]), eps, minPts)
    cl_mat[(1:NROW(a))[!is.na(meter_rl_data_coor[,2*i])], i] = d$cluster
  }
  {
    cl.stream = matrix(0, NROW(a), length(A))
    cl.record = list() 
    cl.list = list() 
    cl.threshold = 0.5 
    i = 1 
  }
  
  cl.compare = function(x,y){length(intersect(x,y))/length(union(x,y))}
  
  while(i != length(A)){
    while(max(cl_mat[, i]) == 0){  if(i != length(A)){i = i+1}else break }
    if(i == length(A)) break
    cl.list = list()
    for(j in 1:max(cl_mat[, i])){
      cl.list[[length(cl.list)+1]] = list(rindex = length(cl.record)+1, 
                                          cluster = which(cl_mat[,i] == j), 
                                          count = 1,
                                          check = 0)
      cl.record[[length(cl.record)+1]] = list(cluster = cl.list[[length(cl.list)]][["cluster"]],
                                              start = i, 
                                              end = NA)
      cl.stream[which(cl_mat[,i] == j), i] = rep(1, length(which(cl_mat[,i] == j)))
      
    }
    
    while(length(cl.list) > 0){
      
      if(i != length(A)){
        
        if(max(cl_mat[,i+1]) > 0){
          cl.list.next = list()
          cl.check.next = rep(0, max(cl_mat[,i+1])) 
          for(k in 1:max(cl_mat[,i+1])){ 
            cl.list.next[[k]] = which(cl_mat[,i+1] == k)
          }
          for(j in 1:length(cl.list)){ 
            compare.result = lapply(cl.list.next, cl.compare, y = cl.list[[j]][["cluster"]])
            if((compare.result > cl.threshold) %>% sum()){
              cl.list[[j]]$cluster = cl.list.next[[which(compare.result > cl.threshold)]] 
              cl.list[[j]]$count = cl.list[[j]]$count + 1 
              cl.list[[j]]$check = 1 
              cl.check.next[which(compare.result > cl.threshold)] = 1 
              cl.record[[cl.list[[j]]$rindex]][['cluster']] = cl.list.next[[which(compare.result > cl.threshold)]]
            }else{ 
              cl.record[[cl.list[[j]]$rindex]][["end"]] = i
            }
          }
          
          cl.list = cl.list[which(unlist(lapply(cl.list, function(x) (x$check))) == 1)]
          new.scene.list = cl.list.next[which(cl.check.next == 0)]
          
          if(length(new.scene.list) > 0){
            for (k in 1:length(new.scene.list)) {
              cl.list = list.append(cl.list, list(rindex = length(cl.record)+1, 
                                                  cluster = new.scene.list[[k]], 
                                                  count = 1,
                                                  check = 0))
              cl.record[[length(cl.record)+1]] = list(cluster = new.scene.list[[k]],
                                                      start = i+1,
                                                      end = NA)
            }
          }
          
          cl.list = lapply(cl.list, replace, "check", 0) 
          i = i+1
          for(j in 1:length(cl.list)){
            cl.stream[cl.list[[j]][['cluster']], i] = rep(cl.list[[j]][['count']], length(cl.list[[j]][['cluster']]))
          }
        }else{ 
          for(r in unlist(lapply(cl.list, function(x) x$rindex))){ cl.record[[r]]$end = i  }
          cl.list = list()
          i = i+1
        }
      }else{ for(r in unlist(lapply(cl.list, function(x) x$rindex))){ cl.record[[r]]$end = i  } 
        cl.list = list() 
      }
    }
  }
  ed.time = Sys.time()
  cat('Total Time:', ed.time - st.time)
}



### dbscan plot
setwd("C:/Users/Tobio/Google 雲端硬碟/2021_Spring/meeting/results/osaka1/KSdbscan")
{
  library(dbscan)
  n_scenes = NCOL(meter_rl_data_coor)/2 # 影像數量
  dbscan.result = list()
  for(i in seq(0, (n_scenes-1), stride)){
    data=na.omit(meter_rl_data_coor[,c(2*i+1,2*i+2)])
    # save cluster scenes
    png(filename = paste0(i/stride+1, ".png"),width = 800,height = 600)
    if(cluster_scenes_result[i,] %>% sum() > 0){
      dbscan.result = dbscan(data, eps, minPts)$cluster
      plot(data, main=paste(paste(colnames(cluster_scenes_result)[which(cluster_scenes_result[i,] == 1)], collapse = ' '),
                            vlength*i/n_scenes), pch=16, cex=3, col = dbscan.result,
           xlim = c(0, 18.331), ylim = c(0, 22.187), cex.main=3)
      points(data[dbscan.result==0,], pch=3, col = "grey", cex=3)
      dev.off()
    }else{
      plot(data, main=paste0(vlength*i/n_scenes), xlim = c(0, 18.331),
           ylim = c(0, 22.187), cex.main=3, cex=3)
      dev.off()
    }
  }
}

### plot coordinate
plot(na.omit(meter_rl_data_coor[,3:4]))

### dbscan no plot !
{
  library(dbscan)
  eps = 3
  minPts = 10
  dbscan.result = c()
  dbscan.result.ratio = c()
  n_scenes = NCOL(meter_rl_data_coor)/2 # 影像數量
  for(i in seq(229, 230, stride)){
    data=na.omit(meter_rl_data_coor[,c(2*i+1,2*i+2)])
    # save cluster scenes
    # png(filename = paste0(i/stride+1, ".png"),width = 800,height = 600)
    dbscan_result = dbscan(data, eps, minPts)
    dbscan.result[i+1] = ifelse(max(dbscan_result$cluster)>0, 1, 0)
    dbscan.result.ratio[i+1] = mean(dbscan_result$cluster>0)
    plot(data, main='', pch=16, cex.axis=1.5,cex=3,col = 'red',
         xlim = c(-2, 16), ylim = c(-2, 16), cex.main=3, xlab="", ylab="")
    points(data[dbscan_result$cluster==0,], pch=16, cex=3)
  }
}

{
  setwd("C:/Users/Tobio/Google 雲端硬碟/2021_Spring/meeting/results/osaka1/locs")
  
  n_scenes = NCOL(meter_rl_data_coor)/2 # 影像數量
  for(i in seq(0, (n_scenes-1), stride)){
    data=na.omit(meter_rl_data_coor[,c(2*i+1,2*i+2)])
    # save cluster scenes
    png(filename = paste0(i/stride+1, ".png"),width = 800,height = 600)
    plot(data, main=paste0(vlength*i/n_scenes), xlim = c(0, 18.331),
         ylim = c(0, 22.187), cex.main=3, cex=3, pch=19)
    dev.off()
    
  }
}

# ### save features
# feature.tc = features
# feature.tc.r = cbind(DS_beta_cluster_ratio ,feature.tc[,26:28])
# feature.osaka = features
# 
# label_K = features[,26]>0
# label_qc = features[,27]>0.95
# label_qm = features[,28]>0.95
# label_DS = DS_beta_cluster_ratio[,10]>0.95
# Label = cbind(label_K, label_qc, label_DS)
# table(apply(Label,1,sum), label)
# 
# table('K' = label_K, 'DS' = label_DS)
# concordance = function(m){
#   return((m[1,1]+m[2,2])/sum(m))
# }
# concordance(table('K' = label_K, 'DS' = label_DS))
# concordance(table('qc' = label_qc, 'DS' = label_DS))
# concordance(table('K' = label_K, 'qc' = label_qc))
# concordance(table('qm' = label_qm, 'DS' = label_DS))
# concordance(table('K' = label_K, 'qm' = label_qm))
# concordance(table('K' = label_K, 'label' = label))
# concordance(table('qc' = label_qc, 'label' = label))
# concordance(table('qm' = label_qm, 'label' = label))
# concordance(table('DS' = label_DS, 'label' = label))


# ### Spatial scan statistic
# if(2 %in% detection.method){
#   test = SSS(data = data, r = 2, perm = 499, alpha = 0.1)
#   cluster_scenes_result[i+1, 'S'] = test$cluster
#   mlc[as.integer(test$index), i+1] = 1
# }


plot(na.omit(meter_rl_data_coor[,c(2*194+1,2*194+2)]),
     main='195', xlim = c(0, 18.331),
     ylim = c(0, 22.187), cex.main=3, cex=3)

### plot coordinates
plot_coordinates = function(data, index){
  for(i in index){
    plot(na.omit(data[,c(2*i+1,2*i+2)]),
         main=paste0(i+1), xlim = c(-2, 16), cex.axis=1.5,
         ylim = c(-2, 16), cex.main=3, cex=3, pch=19, xlab="", ylab="")
  }
  
}
plot_coordinates(meter_rl_data_coor, c(756))
plot_coordinates(meter_rl_data_coor, which((kmed.result$cluster == 1)&(gclabel == 1))-1)

plot_coordinates(meter_rl_data_coor, which(DS_beta_cluster_density[,1]>5)[which(DS_beta_cluster_density[,1]>5)>634])
library(heatmaply)
heatmaply(cbind(DS_beta_cluster_density,DS_beta_cluster_density2),
          Rowv=F,
          Colv=F,
          colors = c('white','yellow','red'))
heatmaply(DS_beta_cluster_ratio_combined,
          Rowv=F,
          Colv=F,
          colors = c('white','yellow','green'))
which(DS_beta_cluster_ratio[,25]>DS_beta_cluster_ratio[,1])

library(gplots) # heatmap.2
heatmap.2(cor(DS_beta_cluster_ratio), trace='none',
          col = colorRampPalette(c('blue','white', 'red'))(16), 
          density.info='none', Rowv = F, Colv = F, srtCol=0, keysize = 1, 
          margins = c(10, 10))
dsr1 = DS_beta_cluster_density[which(DS_beta_cluster_ratio[,25]>DS_beta_cluster_ratio[,1]),25]/DS_beta_cluster_density[which(DS_beta_cluster_ratio[,25]>DS_beta_cluster_ratio[,1]),1]
dsr2 = DS_beta_cluster_density[-which(DS_beta_cluster_ratio[,25]>DS_beta_cluster_ratio[,1]),25]/DS_beta_cluster_density[-which(DS_beta_cluster_ratio[,25]>DS_beta_cluster_ratio[,1]),1]
dsr2 = na.omit(dsr2[dsr2>0])

t.test(dsr1, dsr2)
boxplot(dsr1,dsr2)
max(dsr2)

DS_beta_cluster_density[which(DS_beta_cluster_ratio[,25]>DS_beta_cluster_ratio[,1]),1]
DS_beta_cluster_density[-which(DS_beta_cluster_ratio[,25]>DS_beta_cluster_ratio[,1]),1]
