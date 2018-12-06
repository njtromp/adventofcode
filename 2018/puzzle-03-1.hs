import Data.List

claims :: [((Int, Int),(Int, Int))]
claims = [((596,731), (11,27)), ((20,473), (23,22)), ((730,802), (23,23)), ((212,725), (28,25)), ((65,785), (13,15)), ((495,395), (16,11)), ((750,29), (26,17)), ((658,927), (22,11)), ((109,286), (11,16)), ((935,957), (11,20)), ((647,392), (23,26)), ((878,21), (12,24)), ((816,319), (16,13)), ((339,150), (27,25)), ((770,122), (11,17)), ((439,330), (11,18)), ((727,757), (21,12)), ((936,364), (24,23)), ((78,408), (25,18)), ((579,298), (10,19)), ((573,694), (21,19)), ((121,2), (18,27)), ((843,633), (27,25)), ((948,744), (15,11)), ((313,680), (17,28)), ((192,573), (15,15)), ((126,229), (24,24)), ((238,56), (12,24)), ((168,609), (13,10)), ((488,896), (6,14)), ((194,659), (28,23)), ((71,500), (26,11)), ((525,695), (22,23)), ((334,145), (22,15)), ((167,669), (27,29)), ((329,858), (17,27)), ((543,282), (20,26)), ((261,906), (12,15)), ((683,199), (11,11)), ((775,416), (29,23)), ((507,898), (19,21)), ((409,862), (27,23)), ((328,96), (13,24)), ((333,199), (28,23)), ((152,303), (24,15)), ((150,685), (29,15)), ((460,39), (29,18)), ((820,716), (14,22)), ((241,603), (13,29)), ((815,665), (13,10)), ((144,879), (27,16)), ((852,634), (28,27)), ((712,227), (22,14)), ((809,68), (26,25)), ((154,424), (29,10)), ((428,348), (24,27)), ((98,877), (20,26)), ((346,717), (19,29)), ((523,801), (21,22)), ((876,813), (11,11)), ((467,400), (11,17)), ((185,711), (16,24)), ((544,18), (13,24)), ((933,652), (28,17)), ((869,570), (27,14)), ((917,900), (22,12)), ((542,651), (23,11)), ((713,529), (23,18)), ((653,17), (16,20)), ((197,763), (14,18)), ((769,755), (15,23)), ((672,752), (28,13)), ((427,197), (22,24)), ((314,325), (27,23)), ((447,574), (10,22)), ((430,746), (19,12)), ((849,383), (29,20)), ((285,229), (24,24)), ((415,503), (23,19)), ((272,839), (23,18)), ((292,675), (23,10)), ((558,424), (5,4)), ((690,671), (26,26)), ((373,860), (23,11)), ((940,574), (16,15)), ((687,332), (13,16)), ((121,567), (11,27)), ((831,408), (14,17)), ((47,5), (12,26)), ((270,856), (22,15)), ((292,584), (13,14)), ((532,147), (29,26)), ((846,638), (11,10)), ((272,551), (26,18)), ((717,724), (15,19)), ((793,943), (18,23)), ((428,554), (27,22)), ((942,450), (13,18)), ((70,112), (24,24)), ((98,275), (14,15)), ((303,984), (26,12)), ((664,521), (19,23)), ((773,480), (15,25)), ((666,112), (12,22)), ((589,339), (11,23)), ((756,559), (18,14)), ((447,977), (17,13)), ((802,502), (19,23)), ((821,163), (19,23)), ((447,48), (16,28)), ((606,795), (25,16)), ((326,212), (21,21)), ((477,121), (24,13)), ((470,655), (29,25)), ((247,591), (20,27)), ((412,494), (11,20)), ((732,719), (23,29)), ((319,411), (20,13)), ((138,13), (10,12)), ((551,54), (26,25)), ((95,648), (16,22)), ((595,142), (18,28)), ((884,592), (28,26)), ((322,483), (17,18)), ((381,644), (16,24)), ((778,487), (29,26)), ((272,885), (24,21)), ((939,957), (13,28)), ((624,376), (28,18)), ((735,942), (4,6)), ((985,490), (10,18)), ((611,698), (29,10)), ((655,655), (28,27)), ((768,248), (13,22)), ((769,311), (25,12)), ((20,979), (15,19)), ((182,692), (12,12)), ((172,114), (26,18)), ((322,521), (21,21)), ((623,663), (17,17)), ((719,243), (13,18)), ((281,573), (26,22)), ((880,6), (15,12)), ((276,631), (24,17)), ((43,544), (20,25)), ((717,500), (15,16)), ((63,495), (24,11)), ((943,220), (26,19)), ((197,251), (15,23)), ((461,279), (27,11)), ((790,714), (10,25)), ((858,718), (23,26)), ((167,168), (10,3)), ((403,717), (16,22)), ((662,22), (17,19)), ((949,164), (13,28)), ((570,705), (25,21)), ((441,195), (22,16)), ((796,21), (24,17)), ((883,798), (28,29)), ((901,885), (21,29)), ((574,106), (28,26)), ((745,809), (21,12)), ((375,724), (15,29)), ((461,261), (21,12)), ((898,412), (16,18)), ((281,624), (23,12)), ((446,912), (21,12)), ((410,222), (14,11)), ((170,224), (22,26)), ((763,562), (19,11)), ((203,66), (25,19)), ((493,792), (15,26)), ((337,183), (21,27)), ((523,823), (19,14)), ((947,954), (25,26)), ((371,556), (29,23)), ((407,232), (24,11)), ((792,927), (14,19)), ((7,630), (4,7)), ((608,785), (17,15)), ((783,702), (15,15)), ((350,703), (19,21)), ((650,942), (24,12)), ((610,936), (12,11)), ((873,841), (15,18)), ((960,386), (25,25)), ((876,555), (21,28)), ((370,302), (27,12)), ((504,563), (12,23)), ((900,895), (26,23)), ((122,176), (21,21)), ((596,789), (20,21)), ((887,232), (15,21)), ((307,595), (24,29)), ((579,936), (20,29)), ((549,341), (20,17)), ((211,142), (21,11)), ((797,6), (10,25)), ((928,376), (24,10)), ((577,70), (10,14)), ((851,258), (19,17)), ((244,667), (13,23)), ((448,822), (15,21)), ((196,436), (15,19)), ((817,621), (14,10)), ((142,677), (13,26)), ((688,117), (23,29)), ((96,892), (29,25)), ((912,741), (15,10)), ((578,579), (15,24)), ((219,73), (20,29)), ((949,465), (18,29)), ((867,214), (25,26)), ((52,388), (21,28)), ((142,673), (11,13)), ((55,884), (12,19)), ((793,469), (19,23)), ((544,159), (23,23)), ((225,72), (23,22)), ((966,649), (11,22)), ((431,623), (25,28)), ((274,885), (21,23)), ((718,431), (29,26)), ((325,652), (17,26)), ((304,232), (25,28)), ((932,790), (11,25)), ((489,891), (21,13)), ((769,8), (20,20)), ((166,576), (27,13)), ((473,531), (16,21)), ((833,608), (21,22)), ((544,640), (13,23)), ((303,561), (21,15)), ((874,782), (26,11)), ((808,819), (19,10)), ((573,962), (19,20)), ((472,366), (17,10)), ((477,619), (10,24)), ((734,851), (15,17)), ((442,874), (24,13)), ((231,730), (12,17)), ((296,305), (18,11)), ((556,33), (21,15)), ((585,110), (27,28)), ((814,862), (27,22)), ((608,457), (29,15)), ((670,616), (11,15)), ((717,737), (24,15)), ((160,542), (11,12)), ((136,390), (29,25)), ((360,699), (21,15)), ((878,911), (19,23)), ((559,581), (14,23)), ((204,359), (23,25)), ((431,868), (16,10)), ((826,783), (26,21)), ((650,364), (15,21)), ((770,252), (17,23)), ((816,502), (21,18)), ((426,110), (29,27)), ((705,435), (21,13)), ((193,654), (10,16)), ((249,180), (19,24)), ((389,171), (19,14)), ((88,828), (17,29)), ((676,138), (11,16)), ((407,439), (24,21)), ((914,761), (28,13)), ((976,499), (20,19)), ((611,830), (12,16)), ((5,832), (23,19)), ((653,328), (16,13)), ((837,817), (20,26)), ((861,165), (23,29)), ((362,804), (20,14)), ((456,285), (24,18)), ((208,377), (14,15)), ((413,865), (19,17)), ((799,609), (28,17)), ((882,8), (6,7)), ((590,419), (25,15)), ((30,791), (25,22)), ((923,655), (11,24)), ((156,967), (29,25)), ((594,438), (19,22)), ((769,13), (28,22)), ((177,709), (15,24)), ((597,386), (19,16)), ((360,172), (10,11)), ((960,764), (24,28)), ((439,72), (27,16)), ((431,970), (21,29)), ((291,760), (20,16)), ((11,176), (22,23)), ((320,703), (13,25)), ((388,426), (23,27)), ((221,177), (27,11)), ((947,807), (25,10)), ((668,937), (25,19)), ((472,388), (14,19)), ((174,645), (17,23)), ((94,158), (23,23)), ((875,217), (27,27)), ((109,780), (19,19)), ((306,730), (18,26)), ((301,880), (23,14)), ((601,736), (29,17)), ((664,867), (24,10)), ((966,332), (18,21)), ((699,199), (17,24)), ((283,64), (25,28)), ((520,852), (28,26)), ((577,344), (12,21)), ((220,908), (27,12)), ((41,731), (22,13)), ((14,841), (17,29)), ((444,813), (15,14)), ((437,807), (22,27)), ((453,569), (11,10)), ((725,62), (14,19)), ((861,779), (24,29)), ((825,901), (12,24)), ((69,789), (11,11)), ((81,363), (22,10)), ((944,347), (12,22)), ((20,359), (13,11)), ((156,228), (19,29)), ((847,599), (21,12)), ((967,814), (22,18)), ((441,13), (25,20)), ((581,823), (23,24)), ((624,360), (13,17)), ((274,252), (19,22)), ((140,172), (27,29)), ((52,525), (24,14)), ((511,333), (13,25)), ((333,461), (18,24)), ((722,782), (16,23)), ((186,648), (20,18)), ((131,600), (12,23)), ((524,398), (20,27)), ((819,663), (10,28)), ((595,901), (22,29)), ((950,338), (26,20)), ((635,164), (12,16)), ((653,531), (23,21)), ((386,169), (23,12)), ((74,528), (12,28)), ((872,173), (23,21)), ((506,272), (12,26)), ((387,584), (20,16)), ((701,894), (24,11)), ((974,637), (14,18)), ((540,109), (12,23)), ((480,854), (22,29)), ((492,792), (29,24)), ((727,905), (18,27)), ((236,686), (13,26)), ((273,313), (23,16)), ((303,34), (12,12)), ((455,572), (6,3)), ((122,155), (18,25)), ((546,605), (18,10)), ((936,232), (10,15)), ((17,137), (12,12)), ((795,550), (21,15)), ((67,780), (11,24)), ((568,47), (19,10)), ((402,884), (29,17)), ((439,757), (11,12)), ((462,400), (10,11)), ((741,291), (12,25)), ((392,171), (25,27)), ((290,72), (25,18)), ((161,680), (18,26)), ((404,504), (16,24)), ((552,630), (24,13)), ((252,177), (23,17)), ((782,428), (16,22)), ((35,306), (10,27)), ((904,511), (26,10)), ((152,520), (11,24)), ((40,953), (29,16)), ((579,476), (13,25)), ((293,400), (29,28)), ((106,599), (29,16)), ((420,227), (11,19)), ((334,83), (29,22)), ((503,363), (12,27)), ((715,427), (11,21)), ((535,977), (11,18)), ((129,398), (11,19)), ((50,668), (16,29)), ((596,635), (24,22)), ((429,965), (17,23)), ((218,590), (11,11)), ((130,231), (16,19)), ((517,340), (16,13)), ((160,611), (25,22)), ((187,706), (23,20)), ((402,431), (25,15)), ((448,519), (13,17)), ((732,773), (26,21)), ((336,179), (26,12)), ((833,603), (12,18)), ((331,294), (28,24)), ((619,517), (10,15)), ((585,353), (24,15)), ((847,229), (26,13)), ((13,697), (10,25)), ((183,702), (10,29)), ((967,642), (14,12)), ((833,512), (10,21)), ((489,698), (16,26)), ((463,633), (19,24)), ((902,216), (13,16)), ((14,305), (27,12)), ((62,794), (16,24)), ((883,173), (26,22)), ((680,789), (16,24)), ((459,823), (11,17)), ((247,95), (19,21)), ((542,962), (22,27)), ((817,199), (20,12)), ((448,492), (14,24)), ((701,136), (22,19)), ((438,914), (14,12)), ((232,584), (21,23)), ((280,67), (22,26)), ((316,430), (13,22)), ((674,526), (28,15)), ((250,965), (23,12)), ((87,820), (23,28)), ((487,974), (11,15)), ((392,282), (13,13)), ((385,861), (29,22)), ((554,311), (27,28)), ((676,1), (17,11)), ((191,433), (12,12)), ((559,743), (17,12)), ((466,269), (17,10)), ((206,804), (24,10)), ((609,252), (29,19)), ((212,890), (11,28)), ((404,499), (11,10)), ((229,174), (21,26)), ((321,884), (19,17)), ((600,857), (20,21)), ((334,666), (10,27)), ((129,706), (24,17)), ((569,833), (23,16)), ((613,328), (17,11)), ((206,391), (27,28)), ((25,312), (24,21)), ((647,129), (13,12)), ((885,821), (10,24)), ((799,273), (23,16)), ((67,135), (21,14)), ((454,395), (21,18)), ((947,149), (20,23)), ((281,302), (13,23)), ((68,343), (23,24)), ((53,569), (13,12)), ((20,404), (27,12)), ((803,264), (22,13)), ((309,895), (14,20)), ((111,118), (18,18)), ((339,42), (14,19)), ((687,0), (26,12)), ((430,567), (16,22)), ((0,636), (14,29)), ((111,36), (14,13)), ((205,946), (18,22)), ((483,62), (24,25)), ((752,606), (20,24)), ((406,213), (26,24)), ((795,774), (15,10)), ((966,240), (28,21)), ((76,691), (16,13)), ((830,412), (19,24)), ((530,682), (12,25)), ((866,367), (11,23)), ((725,346), (16,19)), ((346,66), (13,25)), ((298,162), (29,16)), ((234,632), (10,29)), ((895,255), (23,12)), ((293,339), (24,25)), ((95,407), (22,11)), ((310,827), (21,28)), ((880,182), (24,21)), ((784,768), (16,22)), ((811,574), (25,13)), ((499,573), (14,10)), ((641,406), (21,11)), ((67,900), (8,23)), ((691,677), (19,21)), ((548,420), (26,12)), ((547,408), (23,15)), ((102,679), (12,24)), ((874,23), (29,18)), ((587,693), (22,28)), ((36,472), (19,21)), ((890,76), (18,23)), ((635,883), (14,27)), ((610,928), (13,19)), ((615,391), (26,11)), ((786,603), (15,10)), ((538,467), (18,11)), ((500,290), (29,21)), ((157,429), (16,13)), ((180,595), (16,13)), ((715,431), (22,16)), ((308,605), (11,13)), ((848,831), (24,26)), ((404,806), (17,19)), ((155,294), (23,12)), ((889,201), (20,19)), ((764,464), (26,21)), ((971,367), (15,13)), ((142,855), (14,23)), ((538,139), (17,11)), ((883,714), (24,10)), ((102,626), (10,17)), ((724,28), (22,26)), ((850,755), (23,28)), ((651,129), (27,19)), ((451,117), (19,27)), ((300,604), (13,22)), ((686,371), (23,27)), ((955,304), (26,29)), ((909,459), (22,27)), ((863,822), (19,27)), ((432,789), (10,25)), ((35,430), (13,5)), ((546,774), (21,10)), ((191,96), (27,26)), ((815,779), (18,22)), ((224,630), (21,16)), ((667,184), (20,23)), ((743,310), (14,18)), ((656,974), (24,14)), ((316,173), (18,29)), ((57,722), (14,22)), ((301,877), (28,19)), ((15,972), (17,13)), ((713,919), (19,17)), ((263,0), (22,28)), ((778,493), (14,14)), ((130,127), (14,17)), ((933,747), (27,19)), ((920,132), (23,29)), ((774,528), (18,23)), ((227,707), (27,16)), ((464,395), (24,16)), ((556,578), (15,13)), ((717,229), (12,3)), ((704,788), (27,18)), ((330,976), (12,19)), ((538,595), (24,16)), ((750,774), (23,12)), ((558,285), (11,27)), ((639,870), (29,29)), ((389,339), (19,19)), ((21,791), (27,22)), ((540,457), (19,14)), ((800,718), (21,14)), ((836,144), (23,22)), ((818,398), (15,21)), ((422,58), (20,23)), ((376,445), (23,25)), ((266,819), (19,26)), ((338,707), (29,27)), ((930,953), (21,15)), ((818,607), (24,12)), ((612,815), (10,22)), ((352,68), (3,15)), ((684,0), (11,22)), ((765,529), (16,15)), ((643,381), (29,28)), ((572,763), (14,10)), ((597,426), (29,20)), ((953,514), (12,10)), ((67,472), (16,22)), ((275,813), (20,29)), ((790,553), (11,13)), ((956,792), (11,20)), ((290,383), (10,20)), ((697,738), (27,25)), ((97,20), (21,21)), ((61,777), (14,27)), ((311,316), (14,22)), ((81,828), (24,26)), ((464,141), (28,16)), ((869,235), (22,19)), ((883,624), (12,10)), ((270,831), (19,18)), ((81,620), (12,26)), ((305,32), (16,10)), ((743,817), (24,12)), ((289,47), (12,22)), ((309,45), (25,17)), ((854,262), (5,7)), ((670,627), (13,14)), ((88,890), (20,13)), ((923,766), (11,16)), ((942,679), (21,29)), ((602,384), (13,19)), ((47,578), (16,14)), ((938,769), (14,17)), ((418,227), (23,22)), ((593,644), (22,21)), ((926,360), (25,28)), ((394,907), (13,20)), ((729,247), (29,17)), ((830,201), (16,13)), ((814,325), (13,19)), ((466,360), (28,14)), ((368,195), (16,21)), ((206,699), (14,17)), ((106,679), (25,21)), ((789,738), (13,27)), ((254,679), (11,14)), ((182,655), (22,11)), ((210,100), (17,21)), ((223,716), (29,11)), ((830,919), (21,23)), ((112,415), (20,25)), ((459,403), (18,28)), ((871,913), (21,18)), ((208,134), (10,16)), ((566,952), (18,24)), ((368,123), (19,24)), ((879,40), (26,20)), ((17,195), (10,10)), ((321,34), (28,19)), ((881,59), (22,20)), ((76,634), (29,21)), ((533,628), (28,18)), ((667,315), (16,10)), ((312,526), (21,13)), ((595,943), (28,11)), ((323,879), (10,24)), ((601,370), (28,19)), ((909,419), (13,22)), ((923,343), (25,24)), ((438,386), (29,21)), ((633,128), (18,11)), ((420,540), (29,10)), ((482,551), (25,23)), ((883,970), (16,14)), ((689,340), (8,4)), ((486,36), (12,25)), ((741,774), (17,25)), ((914,858), (25,23)), ((309,60), (12,29)), ((354,109), (25,11)), ((264,46), (11,15)), ((961,626), (20,12)), ((835,662), (12,16)), ((827,334), (20,11)), ((739,944), (12,25)), ((369,666), (11,27)), ((248,736), (16,29)), ((237,742), (18,28)), ((222,619), (28,16)), ((699,847), (25,24)), ((180,648), (19,16)), ((312,479), (13,29)), ((827,326), (19,26)), ((918,459), (26,19)), ((160,947), (15,13)), ((198,914), (12,24)), ((521,797), (27,29)), ((133,781), (25,29)), ((464,867), (18,25)), ((460,518), (24,15)), ((840,624), (19,28)), ((876,461), (18,28)), ((868,708), (24,23)), ((865,830), (14,25)), ((789,712), (14,16)), ((333,982), (23,17)), ((445,111), (26,27)), ((960,259), (28,14)), ((435,576), (19,24)), ((89,309), (29,28)), ((313,110), (22,29)), ((387,274), (18,17)), ((567,635), (19,10)), ((796,716), (29,26)), ((447,733), (12,25)), ((561,573), (28,22)), ((400,484), (19,17)), ((897,426), (23,27)), ((549,99), (24,14)), ((614,647), (27,17)), ((926,725), (17,28)), ((562,63), (28,23)), ((428,853), (10,16)), ((31,135), (15,22)), ((970,965), (22,10)), ((397,538), (29,10)), ((801,139), (21,28)), ((305,157), (10,25)), ((428,769), (13,29)), ((38,398), (17,24)), ((740,817), (16,13)), ((574,164), (27,18)), ((804,288), (17,27)), ((546,137), (20,16)), ((96,872), (28,23)), ((26,113), (12,25)), ((290,650), (12,19)), ((666,669), (24,14)), ((752,105), (16,15)), ((269,747), (16,20)), ((951,717), (10,12)), ((481,67), (19,24)), ((243,969), (21,24)), ((632,820), (29,14)), ((540,970), (21,14)), ((366,962), (15,18)), ((140,116), (19,15)), ((638,688), (13,15)), ((511,817), (28,21)), ((187,694), (25,21)), ((213,426), (25,21)), ((602,640), (28,12)), ((493,642), (22,16)), ((702,745), (27,14)), ((906,256), (20,26)), ((936,426), (16,28)), ((633,875), (29,29)), ((869,872), (26,18)), ((763,285), (25,15)), ((749,748), (17,25)), ((76,639), (11,25)), ((306,160), (17,23)), ((954,312), (19,25)), ((804,324), (23,13)), ((619,132), (21,27)), ((800,46), (26,14)), ((902,714), (12,21)), ((148,885), (23,24)), ((125,3), (19,11)), ((935,330), (23,21)), ((737,269), (22,18)), ((140,782), (11,25)), ((767,107), (12,20)), ((334,524), (25,29)), ((456,18), (24,22)), ((435,972), (29,24)), ((272,553), (12,10)), ((823,257), (24,28)), ((242,683), (22,27)), ((415,208), (22,24)), ((889,266), (19,23)), ((318,225), (26,22)), ((885,880), (14,27)), ((598,951), (24,15)), ((748,676), (20,27)), ((296,811), (23,20)), ((55,486), (19,20)), ((724,892), (18,22)), ((475,612), (25,25)), ((365,94), (23,16)), ((466,121), (13,13)), ((537,290), (24,29)), ((474,908), (15,26)), ((982,84), (18,10)), ((271,177), (29,28)), ((768,47), (25,11)), ((875,570), (29,13)), ((734,915), (5,8)), ((745,42), (11,18)), ((735,695), (23,25)), ((763,36), (22,16)), ((898,938), (28,12)), ((676,555), (19,21)), ((594,890), (29,14)), ((239,172), (19,19)), ((551,246), (20,13)), ((208,742), (28,23)), ((186,578), (17,18)), ((24,421), (29,24)), ((975,649), (10,13)), ((510,853), (27,22)), ((572,932), (13,26)), ((551,77), (19,14)), ((156,866), (11,25)), ((574,982), (11,17)), ((282,551), (10,16)), ((634,860), (26,21)), ((960,856), (15,15)), ((782,727), (12,16)), ((887,177), (21,15)), ((165,161), (15,16)), ((288,762), (14,10)), ((260,184), (23,20)), ((630,699), (11,12)), ((363,652), (15,24)), ((961,629), (17,25)), ((941,853), (16,29)), ((489,978), (25,21)), ((70,537), (20,11)), ((668,525), (29,17)), ((396,429), (26,26)), ((10,791), (12,23)), ((417,967), (21,10)), ((270,506), (17,12)), ((106,425), (22,24)), ((160,10), (23,10)), ((369,113), (10,19)), ((498,644), (27,10)), ((47,468), (21,21)), ((429,200), (12,3)), ((909,878), (21,21)), ((679,837), (29,10)), ((565,553), (24,15)), ((65,898), (14,28)), ((327,58), (23,12)), ((700,787), (26,28)), ((450,664), (19,10)), ((6,569), (23,26)), ((889,183), (10,27)), ((429,37), (28,12)), ((576,598), (10,29)), ((800,488), (16,20)), ((693,362), (23,29)), ((516,803), (23,23)), ((351,379), (23,15)), ((396,292), (27,27)), ((474,713), (21,26)), ((968,566), (12,23)), ((976,344), (18,27)), ((112,124), (15,12)), ((960,381), (13,16)), ((726,748), (16,17)), ((755,385), (12,24)), ((288,584), (15,29)), ((47,660), (11,21)), ((340,842), (20,26)), ((58,879), (16,16)), ((963,240), (11,10)), ((559,226), (22,27)), ((553,578), (25,24)), ((928,848), (25,19)), ((116,30), (13,12)), ((670,253), (23,10)), ((119,493), (10,26)), ((667,663), (21,16)), ((104,591), (20,18)), ((242,893), (20,18)), ((508,381), (18,28)), ((417,720), (12,17)), ((104,691), (19,17)), ((673,843), (27,18)), ((468,647), (24,19)), ((716,762), (20,27)), ((915,758), (13,16)), ((472,153), (14,24)), ((400,960), (27,12)), ((156,945), (13,10)), ((481,916), (29,18)), ((361,799), (20,22)), ((833,388), (22,24)), ((501,281), (18,18)), ((91,834), (20,23)), ((637,49), (26,13)), ((878,935), (27,21)), ((118,854), (15,14)), ((412,802), (11,22)), ((601,688), (15,16)), ((475,887), (16,19)), ((534,420), (11,10)), ((160,653), (15,29)), ((462,193), (23,16)), ((443,632), (24,18)), ((763,316), (14,25)), ((354,87), (18,22)), ((971,251), (21,16)), ((150,570), (25,27)), ((347,150), (10,27)), ((847,926), (11,28)), ((724,331), (12,27)), ((111,300), (28,29)), ((478,916), (10,19)), ((182,628), (24,26)), ((189,802), (22,22)), ((410,207), (28,16)), ((395,905), (16,16)), ((970,562), (27,24)), ((871,814), (20,26)), ((10,147), (14,15)), ((965,650), (15,20)), ((292,236), (29,23)), ((220,490), (21,21)), ((313,684), (22,22)), ((562,593), (7,6)), ((948,967), (17,28)), ((304,304), (10,29)), ((282,60), (11,15)), ((636,676), (11,19)), ((110,177), (21,13)), ((476,296), (28,13)), ((282,744), (20,16)), ((894,411), (14,17)), ((67,773), (20,17)), ((306,614), (14,21)), ((659,874), (27,14)), ((816,812), (17,21)), ((752,14), (20,19)), ((938,567), (13,17)), ((575,455), (11,27)), ((116,9), (12,13)), ((728,946), (11,18)), ((551,544), (28,18)), ((350,296), (16,26)), ((88,788), (27,15)), ((892,512), (22,29)), ((589,676), (26,23)), ((411,192), (24,19)), ((531,658), (20,14)), ((485,723), (22,11)), ((43,18), (14,21)), ((417,128), (14,15)), ((457,15), (16,21)), ((121,182), (10,11)), ((698,930), (16,14)), ((74,693), (28,11)), ((194,932), (14,21)), ((584,674), (13,29)), ((765,767), (13,21)), ((445,330), (18,27)), ((934,852), (16,28)), ((467,911), (17,13)), ((599,793), (27,27)), ((836,120), (17,23)), ((593,63), (14,10)), ((397,405), (16,17)), ((539,343), (19,16)), ((198,957), (10,23)), ((872,353), (11,27)), ((776,274), (15,29)), ((816,584), (19,13)), ((541,838), (29,18)), ((360,706), (10,12)), ((261,171), (20,29)), ((344,681), (10,29)), ((18,848), (28,19)), ((693,798), (25,27)), ((94,159), (14,21)), ((142,4), (23,16)), ((651,782), (23,17)), ((327,520), (13,27)), ((295,753), (27,17)), ((340,181), (24,17)), ((537,277), (28,22)), ((201,332), (12,22)), ((103,117), (12,25)), ((312,444), (16,26)), ((779,505), (21,12)), ((980,623), (11,10)), ((298,48), (13,17)), ((669,529), (22,15)), ((773,758), (17,17)), ((645,127), (26,17)), ((357,979), (12,14)), ((146,155), (15,28)), ((36,654), (12,22)), ((869,19), (20,24)), ((380,629), (28,25)), ((671,310), (21,12)), ((496,655), (23,20)), ((204,359), (22,12)), ((935,123), (29,14)), ((200,431), (24,23)), ((838,674), (17,20)), ((796,25), (23,15)), ((362,377), (13,22)), ((198,593), (29,19)), ((549,165), (15,25)), ((710,78), (22,10)), ((903,230), (10,26)), ((89,891), (11,12)), ((890,935), (24,16)), ((600,60), (12,29)), ((449,735), (3,20)), ((317,588), (11,24)), ((834,68), (14,11)), ((886,964), (23,12)), ((512,772), (18,29)), ((178,900), (18,12)), ((943,354), (23,10)), ((399,226), (11,12)), ((294,546), (28,28)), ((669,981), (12,19)), ((726,815), (17,13)), ((199,449), (17,24)), ((268,850), (10,12)), ((828,391), (22,15)), ((536,406), (20,29)), ((566,843), (22,27)), ((448,792), (10,29)), ((430,18), (12,27)), ((764,923), (20,25)), ((101,95), (12,29)), ((238,955), (13,23)), ((42,322), (25,19)), ((305,87), (15,12)), ((815,432), (22,21)), ((668,564), (26,27)), ((579,55), (15,28)), ((233,15), (10,20)), ((719,843), (21,16)), ((10,403), (19,23)), ((318,844), (26,21)), ((795,32), (22,27)), ((205,350), (17,21)), ((857,475), (10,28)), ((565,964), (18,27)), ((732,53), (20,13)), ((314,837), (14,13)), ((762,104), (14,18)), ((567,397), (11,12)), ((724,815), (26,29)), ((421,305), (26,29)), ((951,381), (24,10)), ((373,110), (29,25)), ((331,707), (28,13)), ((469,21), (26,19)), ((392,225), (24,17)), ((94,864), (29,15)), ((683,658), (17,23)), ((836,650), (26,29)), ((217,477), (27,22)), ((230,13), (17,26)), ((156,972), (29,11)), ((646,855), (23,16)), ((77,779), (21,28)), ((88,685), (28,10)), ((159,901), (20,23)), ((172,916), (22,18)), ((305,353), (13,12)), ((236,192), (29,10)), ((448,410), (22,13)), ((11,633), (14,19)), ((374,739), (15,11)), ((869,648), (25,27)), ((610,267), (25,17)), ((375,183), (21,26)), ((323,497), (12,23)), ((717,490), (10,24)), ((574,627), (24,17)), ((396,308), (24,28)), ((169,671), (19,25)), ((103,779), (10,28)), ((733,934), (12,26)), ((870,606), (17,13)), ((309,244), (28,21)), ((27,958), (27,10)), ((312,907), (17,22)), ((428,359), (12,14)), ((265,6), (17,18)), ((545,219), (28,24)), ((407,910), (16,22)), ((90,515), (21,18)), ((387,555), (15,20)), ((430,580), (11,18)), ((803,498), (11,12)), ((579,662), (22,28)), ((895,253), (16,24)), ((190,615), (25,22)), ((301,310), (12,12)), ((911,261), (29,15)), ((901,891), (17,14)), ((321,694), (27,19)), ((774,945), (24,25)), ((477,884), (21,19)), ((764,608), (25,12)), ((384,415), (18,21)), ((421,357), (14,16)), ((950,792), (22,14)), ((354,83), (17,16)), ((682,142), (29,18)), ((246,32), (25,27)), ((543,783), (12,10)), ((212,440), (13,18)), ((642,774), (17,18)), ((685,914), (18,29)), ((311,986), (29,10)), ((664,340), (16,26)), ((555,134), (27,24)), ((770,782), (14,18)), ((17,720), (26,26)), ((60,556), (23,29)), ((497,295), (25,29)), ((426,782), (10,17)), ((255,515), (23,14)), ((360,213), (11,26)), ((746,21), (26,11)), ((18,571), (26,20)), ((411,323), (23,21)), ((512,827), (20,20)), ((316,258), (12,14)), ((549,244), (28,19)), ((560,732), (15,12)), ((666,121), (11,14)), ((702,840), (14,11)), ((395,435), (16,16)), ((452,569), (21,17)), ((819,615), (27,29)), ((241,546), (21,24)), ((82,897), (27,16)), ((192,585), (13,18)), ((822,850), (17,20)), ((750,106), (19,15)), ((734,19), (24,28)), ((920,887), (14,10)), ((753,702), (18,21)), ((71,491), (28,20)), ((138,24), (14,23)), ((185,920), (25,26)), ((668,625), (20,22)), ((862,469), (15,20)), ((199,131), (23,25)), ((775,483), (13,11)), ((831,490), (28,16)), ((325,213), (7,7)), ((428,478), (24,16)), ((871,549), (23,17)), ((153,866), (19,26)), ((972,77), (17,10)), ((377,159), (18,15)), ((407,913), (28,13)), ((720,937), (22,13)), ((163,864), (27,28)), ((145,674), (25,10)), ((699,222), (23,11)), ((198,150), (20,29)), ((440,667), (12,16)), ((749,36), (22,22)), ((79,783), (11,12)), ((502,365), (19,23)), ((476,893), (27,21)), ((415,21), (25,22)), ((747,952), (12,15)), ((675,513), (21,25)), ((932,453), (24,15)), ((305,575), (23,12)), ((830,132), (10,21)), ((425,372), (28,13)), ((192,645), (28,29)), ((36,373), (13,19)), ((582,762), (22,29)), ((626,462), (27,22)), ((537,277), (14,17)), ((251,543), (23,20)), ((172,224), (28,11)), ((78,529), (22,16)), ((263,49), (29,19)), ((598,350), (12,13)), ((655,28), (18,29)), ((225,157), (12,23)), ((108,521), (16,11)), ((102,685), (22,12)), ((942,513), (20,20)), ((595,686), (23,12)), ((764,385), (12,16)), ((820,128), (23,12)), ((732,787), (21,18)), ((379,577), (16,11)), ((617,832), (27,10)), ((130,689), (11,25)), ((9,349), (14,15)), ((784,23), (13,21)), ((194,252), (13,23)), ((665,235), (23,27)), ((595,463), (23,16)), ((966,301), (14,12)), ((657,897), (16,13)), ((616,329), (23,25)), ((322,620), (26,27)), ((668,758), (21,11)), ((955,668), (15,12)), ((947,381), (20,28)), ((709,522), (24,22)), ((778,587), (27,20)), ((809,405), (14,12)), ((403,221), (15,17)), ((657,604), (22,19)), ((539,600), (20,12)), ((118,487), (22,18)), ((797,37), (10,12)), ((743,101), (29,11)), ((643,147), (21,15)), ((700,6), (19,12)), ((213,618), (13,18)), ((324,262), (16,24)), ((594,452), (26,17)), ((457,295), (25,18)), ((938,260), (15,10)), ((616,518), (15,23)), ((147,883), (21,29)), ((593,636), (12,26)), ((919,785), (20,27)), ((305,388), (18,27)), ((558,405), (14,19)), ((793,10), (15,29)), ((694,919), (22,25)), ((108,610), (16,25)), ((386,347), (14,12)), ((119,22), (22,18)), ((958,726), (15,16)), ((40,654), (19,19)), ((638,949), (17,10)), ((885,835), (16,10)), ((392,182), (26,19)), ((841,937), (17,10)), ((851,803), (29,18)), ((492,723), (14,18)), ((412,209), (19,8)), ((261,550), (27,22)), ((131,683), (20,20)), ((2,626), (14,25)), ((286,288), (18,27)), ((14,802), (10,10)), ((437,928), (16,14)), ((912,912), (27,28)), ((572,297), (15,17)), ((323,211), (12,12)), ((963,764), (15,12)), ((37,368), (28,20)), ((841,592), (18,22)), ((324,739), (23,17)), ((880,629), (19,28)), ((775,476), (14,20)), ((918,880), (21,12)), ((248,976), (26,22)), ((296,661), (13,23)), ((907,790), (15,14)), ((829,344), (15,14)), ((109,802), (15,11)), ((325,230), (10,12)), ((247,105), (26,19)), ((395,473), (14,29)), ((157,221), (16,29)), ((908,886), (19,14)), ((442,929), (25,12)), ((492,784), (19,25)), ((306,684), (16,28)), ((366,266), (27,19)), ((602,634), (14,13)), ((140,863), (28,11)), ((614,413), (29,23)), ((12,814), (10,27)), ((735,279), (26,10)), ((719,745), (14,13)), ((605,842), (10,21)), ((112,680), (22,28)), ((782,518), (25,11)), ((950,847), (13,13)), ((549,947), (11,18)), ((850,773), (23,22)), ((95,29), (17,28))]
-- claims = [((1,3),(4,4)),((3,1),(4,4)),((5,5),(2,2))]

-- countClaims :: [((Int, Int),(Int, Int))] -> Int -> Int -> Int
-- countClaims claims c r = length [ 1 | ((c',r'),(w,h)) <- claims, c >= c' && c < c' + w && r >= r' && r < r' + h ]

-- claimed :: [[Int]]
-- claimed = [[ 1 | c <- [0..999], countClaims claims c r > 1] | r <- [0..999]]

-- puzzle5 :: Int
-- puzzle5 = sum (map length claimed)

claimed :: [(Int, Int)]
claimed = [ (c',r') | ((c,r),(w,h)) <- claims, c' <- [c..c+w-1], r' <- [r..r+h-1] ]

multipleClaims :: [[(Int, Int)]]
multipleClaims = group (sort claimed)

puzzle_03_1 :: Int
puzzle_03_1 = length (filter (\x -> length x > 1) multipleClaims )