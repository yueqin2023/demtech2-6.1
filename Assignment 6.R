
##input the nLx values provided
nLx <- read.csv("nLx_values.csv", col.names = c("cat_race_ed_age", "nLx"))
nLx$nLx <- as.numeric(nLx$nLx)

##this is provided in Table 2 but seems useless
fx <- matrix(c(2.18, 2.18, 1.67, 1.43, 1.01, 1.76, 1.81, 1.62, 1.54, 1.27), nrow=5, ncol=2)
##instead,I find age-specific fertility rates in Mare's Table 3 and Table 2 useful, so I input them here
fblack <- matrix(c(0, 0, 0, 0.0815, 0.1199, 0.1019, 0.0733, 0.042, 0.0172, 0, 0, 0, 0, 0.0922, 0.1317, 0.0957, 0.0640, 0.0376, 0.0156, 0, 0, 0, 0, 0.0376, 0.1123, 0.0872, 0.0537, 0.0306, 0.0119, 0, 0, 0, 0, 0.0205, 0.0951, 0.0778, 0.0522, 0.0283, 0.0129, 0, 0, 0, 0, 0.0079, 0.0481, 0.0665, 0.0456, 0.0253, 0.0079, 0), nrow=10, ncol=5)


fwhite <- matrix(c(0, 0, 0, 0.0623, 0.1053, 0.0868, 0.0562, 0.0304, 0.0106, 0, 0, 0, 0, 0.0759, 0.1237, 0.0816, 0.0474, 0.0247, 0.0079, 0, 0, 0, 0, 0.0244, 0.1168, 0.0938, 0.0546, 0.0271, 0.0083, 0, 0, 0, 0, 0.008, 0.0974, 0.1038, 0.0601, 0.029, 0.0095, 0, 0, 0, 0, 0.0023, 0.0449, 0.0993, 0.0679, 0.031, 0.0092, 0), nrow=10, ncol=5)


##input proportion of mobility 
mwhite <- matrix(c(0.132, 0.034, 0.014, 0.01, 0.001, 
                   0.179, 0.15, 0.061, 0.027, 0.033,
                   0.485, 0.427, 0.457, 0.243, 0.143, 
                   0.13, 0.208, 0.25, 0.338, 0.259, 
                   0.075, 0.18, 0.217, 0.381, 0.564), nrow = 5, ncol = 5)

mblack <- matrix(c(0.289, 0.079, 0.025, 0.033, 0, 
                   0.268, 0.35, 0.19, 0.038, 0.032, 
                   0.243, 0.278, 0.386, 0.243, 0.163, 
                   0.126, 0.164, 0.212, 0.496, 0.371, 
                   0.073, 0.129, 0.188, 0.189, 0.434), nrow = 5, ncol = 5)

##nLx re-arranged by education and age groups; row is edu, column is age groups 
Lblack <- matrix(c(286188, 302932, 345975, 401302, 436487, 224755, 244586, 297310, 368415, 415788, 211082, 231061, 284923, 359120, 409459, 197355, 217375, 272131, 349230, 402445, 181123, 201066, 256565, 336800, 393143, 163593, 183300, 239235, 322539, 382150, 145842, 165133, 221079, 307131, 370059, 128308, 146987, 202427, 290708, 356890, 111728, 129594, 183918, 273585, 342620, 96459, 113319, 165484, 255751, 326868), nrow = 5, ncol = 10)

Lwhite <- matrix(c(358423, 391479, 427946, 458227, 464829, 312996, 355535, 403919, 446125, 455230, 301145, 345557, 396763, 442054, 451875, 288829, 335002, 389000, 437407, 447994, 273746, 321825, 379009, 431059, 442612, 256847, 306795, 367341, 423362, 436017, 239020, 290646, 354531, 414655, 428482, 220558, 273552, 340596, 404800, 419831, 202046, 255899, 325613, 393503, 409690, 183737, 237756, 309320, 380112, 397315), nrow = 5, ncol = 10)

##later will be used when calculating birth sub-matrix (we need to account for the starting population and for the 1/2 that shows up because our fertility formulas implicitly rely on the mid-period population.)
Lwhite <- Lwhite/(2*100000)

Lblack <- Lblack/(2*100000)

##calculating survival sub-matrix
sblack0 <- matrix(c(nLx$nLx[2]/nLx$nLx[1], 0,0,0,0,
                    0,nLx$nLx[12]/nLx$nLx[11],0,0,0, 0,0,nLx$nLx[22]/nLx$nLx[21],0,0, 0,0,0,nLx$nLx[32]/nLx$nLx[31],0, 0,0,0,0,nLx$nLx[42]/nLx$nLx[41]),nrow=5, ncol=5, byrow=TRUE)


swhite0 <- matrix(c(nLx$nLx[52]/nLx$nLx[51], 0,0,0,0,
                    0,nLx$nLx[62]/nLx$nLx[61],0,0,0, 0,0,nLx$nLx[72]/nLx$nLx[71],0,0, 0,0,0,nLx$nLx[82]/nLx$nLx[81],0, 0,0,0,0,nLx$nLx[92]/nLx$nLx[91]), nrow=5, ncol=5, byrow=TRUE)


sblack5 <- matrix(c(nLx$nLx[3]/nLx$nLx[2], 0,0,0,0,
                    0,nLx$nLx[13]/nLx$nLx[12],0,0,0, 0,0,nLx$nLx[23]/nLx$nLx[22],0,0, 0,0,0,nLx$nLx[33]/nLx$nLx[32],0, 0,0,0,0,nLx$nLx[43]/nLx$nLx[42]),nrow=5, ncol=5, byrow=TRUE)

swhite5 <- matrix(c(nLx$nLx[53]/nLx$nLx[52], 0,0,0,0,
                    0,nLx$nLx[63]/nLx$nLx[62],0,0,0, 0,0,nLx$nLx[73]/nLx$nLx[72],0,0, 0,0,0,nLx$nLx[83]/nLx$nLx[82],0, 0,0,0,0,nLx$nLx[93]/nLx$nLx[92]),nrow=5, ncol=5, byrow=TRUE)

sblack10 <- matrix(c(nLx$nLx[4]/nLx$nLx[3], 0,0,0,0,
                    0,nLx$nLx[14]/nLx$nLx[13],0,0,0, 0,0,nLx$nLx[24]/nLx$nLx[23],0,0, 0,0,0,nLx$nLx[34]/nLx$nLx[33],0, 0,0,0,0,nLx$nLx[44]/nLx$nLx[43]),nrow=5, ncol=5, byrow=TRUE)

swhite10 <- matrix(c(nLx$nLx[54]/nLx$nLx[53], 0,0,0,0,
                    0,nLx$nLx[64]/nLx$nLx[63],0,0,0, 0,0,nLx$nLx[74]/nLx$nLx[73],0,0, 0,0,0,nLx$nLx[84]/nLx$nLx[83],0, 0,0,0,0,nLx$nLx[94]/nLx$nLx[93]),nrow=5, ncol=5, byrow=TRUE)


sblack15 <- matrix(c(nLx$nLx[5]/nLx$nLx[4], 0,0,0,0,
                     0,nLx$nLx[15]/nLx$nLx[14],0,0,0, 0,0,nLx$nLx[25]/nLx$nLx[24],0,0, 0,0,0,nLx$nLx[35]/nLx$nLx[34],0, 0,0,0,0,nLx$nLx[45]/nLx$nLx[44]),nrow=5, ncol=5, byrow=TRUE)

swhite15 <- matrix(c(nLx$nLx[55]/nLx$nLx[54], 0,0,0,0,
                     0,nLx$nLx[65]/nLx$nLx[64],0,0,0, 0,0,nLx$nLx[75]/nLx$nLx[74],0,0, 0,0,0,nLx$nLx[85]/nLx$nLx[84],0, 0,0,0,0,nLx$nLx[95]/nLx$nLx[94]),nrow=5, ncol=5, byrow=TRUE)


sblack20 <- matrix(c(nLx$nLx[6]/nLx$nLx[5], 0,0,0,0,
                     0,nLx$nLx[16]/nLx$nLx[15],0,0,0, 0,0,nLx$nLx[26]/nLx$nLx[25],0,0, 0,0,0,nLx$nLx[36]/nLx$nLx[35],0, 0,0,0,0,nLx$nLx[46]/nLx$nLx[45]),nrow=5, ncol=5, byrow=TRUE)

swhite20 <- matrix(c(nLx$nLx[56]/nLx$nLx[55], 0,0,0,0,
                     0,nLx$nLx[66]/nLx$nLx[65],0,0,0, 0,0,nLx$nLx[76]/nLx$nLx[75],0,0, 0,0,0,nLx$nLx[86]/nLx$nLx[85],0, 0,0,0,0,nLx$nLx[96]/nLx$nLx[95]),nrow=5, ncol=5, byrow=TRUE)

sblack25 <- matrix(c(nLx$nLx[7]/nLx$nLx[6], 0,0,0,0,
                     0,nLx$nLx[17]/nLx$nLx[16],0,0,0, 0,0,nLx$nLx[27]/nLx$nLx[26],0,0, 0,0,0,nLx$nLx[37]/nLx$nLx[36],0, 0,0,0,0,nLx$nLx[47]/nLx$nLx[46]),nrow=5, ncol=5, byrow=TRUE)

swhite25 <- matrix(c(nLx$nLx[57]/nLx$nLx[56], 0,0,0,0,
                     0,nLx$nLx[67]/nLx$nLx[66],0,0,0, 0,0,nLx$nLx[77]/nLx$nLx[76],0,0, 0,0,0,nLx$nLx[87]/nLx$nLx[86],0, 0,0,0,0,nLx$nLx[97]/nLx$nLx[96]),nrow=5, ncol=5, byrow=TRUE)


sblack30 <- matrix(c(nLx$nLx[8]/nLx$nLx[7], 0,0,0,0,
                     0,nLx$nLx[18]/nLx$nLx[17],0,0,0, 0,0,nLx$nLx[28]/nLx$nLx[27],0,0, 0,0,0,nLx$nLx[38]/nLx$nLx[37],0, 0,0,0,0,nLx$nLx[48]/nLx$nLx[47]),nrow=5, ncol=5, byrow=TRUE)

swhite30 <- matrix(c(nLx$nLx[58]/nLx$nLx[57], 0,0,0,0,
                     0,nLx$nLx[68]/nLx$nLx[67],0,0,0, 0,0,nLx$nLx[78]/nLx$nLx[77],0,0, 0,0,0,nLx$nLx[88]/nLx$nLx[87],0, 0,0,0,0,nLx$nLx[98]/nLx$nLx[97]),nrow=5, ncol=5, byrow=TRUE)


sblack35 <- matrix(c(nLx$nLx[9]/nLx$nLx[8], 0,0,0,0,
                     0,nLx$nLx[19]/nLx$nLx[18],0,0,0, 0,0,nLx$nLx[29]/nLx$nLx[28],0,0, 0,0,0,nLx$nLx[39]/nLx$nLx[38],0, 0,0,0,0,nLx$nLx[49]/nLx$nLx[48]),nrow=5, ncol=5, byrow=TRUE)

swhite35 <- matrix(c(nLx$nLx[59]/nLx$nLx[58], 0,0,0,0,
                     0,nLx$nLx[69]/nLx$nLx[68],0,0,0, 0,0,nLx$nLx[79]/nLx$nLx[78],0,0, 0,0,0,nLx$nLx[89]/nLx$nLx[88],0, 0,0,0,0,nLx$nLx[99]/nLx$nLx[98]),nrow=5, ncol=5, byrow=TRUE)


sblack40 <- matrix(c(nLx$nLx[10]/nLx$nLx[9], 0,0,0,0,
                     0,nLx$nLx[20]/nLx$nLx[19],0,0,0, 0,0,nLx$nLx[30]/nLx$nLx[29],0,0, 0,0,0,nLx$nLx[40]/nLx$nLx[39],0, 0,0,0,0,nLx$nLx[50]/nLx$nLx[49]),nrow=5, ncol=5, byrow=TRUE)

swhite40 <- matrix(c(nLx$nLx[60]/nLx$nLx[59], 0,0,0,0,
                     0,nLx$nLx[70]/nLx$nLx[69],0,0,0, 0,0,nLx$nLx[80]/nLx$nLx[79],0,0, 0,0,0,nLx$nLx[90]/nLx$nLx[89],0, 0,0,0,0,nLx$nLx[100]/nLx$nLx[99]),nrow=5, ncol=5, byrow=TRUE)

##calculating birth sub-matrix

bwhite15 <- Lwhite[,1]*(fwhite[4,]+swhite15[1,]*fwhite[5,])*mwhite

bblack15 <- Lblack[,1]*(fblack[4,]+sblack15[1,]*fblack[5,])*mblack

bwhite20 <- Lwhite[,1]*(fwhite[5,]+swhite20[1,]*fwhite[6,])*mwhite

bblack20 <- Lblack[,1]*(fblack[5,]+sblack20[1,]*fblack[6,])*mblack

bwhite25 <- Lwhite[,1]*(fwhite[6,]+swhite25[1,]*fwhite[7,])*mwhite

bblack25 <- Lblack[,1]*(fblack[6,]+sblack25[1,]*fblack[7,])*mblack

bwhite30 <- Lwhite[,1]*(fwhite[7,]+swhite30[1,]*fwhite[8,])*mwhite

bblack30 <- Lblack[,1]*(fblack[7,]+sblack30[1,]*fblack[8,])*mblack

bwhite35 <- Lwhite[,1]*(fwhite[8,]+swhite35[1,]*fwhite[9,])*mwhite

bblack35 <- Lblack[,1]*(fblack[8,]+sblack35[1,]*fblack[9,])*mblack

bwhite40 <- Lwhite[,1]*(fwhite[9,]+swhite40[1,]*fwhite[10,])*mwhite

bblack40 <- Lblack[,1]*(fblack[9,]+sblack40[1,]*fblack[10,])*mblack


##get M matrix
zm = matrix(0,5,5)

row1 = cbind(zm, zm, zm, bwhite15, bwhite20, bwhite25, bwhite30, bwhite35, bwhite40, zm)
row2 = cbind(swhite0, zm, zm, zm, zm, zm, zm, zm, zm, zm)
row3 = cbind(zm, swhite5, zm, zm, zm, zm, zm, zm, zm, zm)
row4 = cbind(zm, zm, swhite10, zm, zm, zm, zm, zm, zm, zm)
row5 = cbind(zm, zm, zm, swhite15, zm, zm, zm, zm, zm, zm)
row6 = cbind(zm, zm, zm, zm, swhite20, zm, zm, zm, zm, zm)
row7 = cbind(zm, zm, zm, zm, zm, swhite25, zm, zm, zm, zm)
row8 = cbind(zm, zm, zm, zm, zm, zm, swhite30, zm, zm, zm)
row9 = cbind(zm, zm, zm, zm, zm, zm, zm, swhite35, zm, zm)
row10 = cbind(zm, zm, zm, zm, zm, zm, zm, zm, swhite40, zm)

M_White = rbind(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10)



row1 = cbind(zm, zm, zm, bblack15, bblack20, bblack25, bblack30, bblack35, bblack40, zm)
row2 = cbind(sblack0, zm, zm, zm, zm, zm, zm, zm, zm, zm)
row3 = cbind(zm, sblack5, zm, zm, zm, zm, zm, zm, zm, zm)
row4 = cbind(zm, zm, sblack10, zm, zm, zm, zm, zm, zm, zm)
row5 = cbind(zm, zm, zm, sblack15, zm, zm, zm, zm, zm, zm)
row6 = cbind(zm, zm, zm, zm, sblack20, zm, zm, zm, zm, zm)
row7 = cbind(zm, zm, zm, zm, zm, sblack25, zm, zm, zm, zm)
row8 = cbind(zm, zm, zm, zm, zm, zm, sblack30, zm, zm, zm)
row9 = cbind(zm, zm, zm, zm, zm, zm, zm, sblack35, zm, zm)
row10 = cbind(zm, zm, zm, zm, zm, zm, zm, zm, sblack40, zm)

M_black = rbind(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10)

