module Day12 where

import Text.Trifecta
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.DFS

part1 = map length $ filter (0 `elem`) $ scc $ graph gr
  where (Success gr) = parseInput input
part2 = length $ scc $ graph gr
  where (Success gr) = parseInput input

graph :: [(Node, [Edge])] -> Gr () ()
graph ns = mkUGraph (map fst ns) (concatMap snd ns)

parseInput :: String -> Result [(Node, [Edge])]
parseInput = parseString (some (nodeP <* skipOptional newline)) mempty

nodeP :: Parser (Node, [Edge])
nodeP = mkN <$> token natural <*> (token (string "<->") *> commaSep1 natural)
  where 
    mkN n e = (intN, zip (map fromIntegral e) (repeat intN))
      where intN = fromIntegral n

test = "0 <-> 2\n1 <-> 1\n2 <-> 0, 3, 4\n3 <-> 2, 4\n4 <-> 2, 3, 6\n5 <-> 6\n6 <-> 4, 5"
input = "0 <-> 584, 830\n1 <-> 415, 531\n2 <-> 514, 1419\n3 <-> 3\n4 <-> 1043\n5 <-> 440\n6 <-> 452, 1651, 1861\n7 <-> 368, 783, 1364\n8 <-> 813\n9 <-> 79, 1562\n10 <-> 465, 1947\n11 <-> 408, 1095\n12 <-> 12, 1386\n13 <-> 179, 497, 1196, 1918\n14 <-> 1650\n15 <-> 211, 1763\n16 <-> 598, 809, 1659\n17 <-> 17, 222, 1233, 1716\n18 <-> 254, 521\n19 <-> 1928\n20 <-> 477, 1388\n21 <-> 133\n22 <-> 1410\n23 <-> 502\n24 <-> 142, 658, 1177\n25 <-> 1975\n26 <-> 1639\n27 <-> 719\n28 <-> 267\n29 <-> 1239\n30 <-> 951, 997, 1814\n31 <-> 750, 1073\n32 <-> 191, 567, 1501\n33 <-> 312\n34 <-> 801\n35 <-> 740\n36 <-> 883, 1039\n37 <-> 37\n38 <-> 1073\n39 <-> 823, 1375, 1790\n40 <-> 618, 987\n41 <-> 188, 988\n42 <-> 1805, 1994\n43 <-> 870, 969, 1107, 1110\n44 <-> 1623\n45 <-> 551, 1745\n46 <-> 634, 790, 1637\n47 <-> 314, 458\n48 <-> 493, 1347, 1573\n49 <-> 339, 1539\n50 <-> 158, 308\n51 <-> 176, 877\n52 <-> 1191, 1236, 1616\n53 <-> 900\n54 <-> 188\n55 <-> 336, 1134\n56 <-> 422, 1110\n57 <-> 817\n58 <-> 862\n59 <-> 373, 732\n60 <-> 1431, 1974\n61 <-> 1246, 1564\n62 <-> 1015, 1227\n63 <-> 166, 234, 288, 1746\n64 <-> 1183\n65 <-> 472\n66 <-> 184, 1979\n67 <-> 1430\n68 <-> 1416, 1674, 1883\n69 <-> 309, 675\n70 <-> 1230\n71 <-> 145, 343\n72 <-> 446, 1185, 1187, 1421, 1819\n73 <-> 248, 1278, 1779\n74 <-> 502, 1470, 1857\n75 <-> 645, 914\n76 <-> 800, 968\n77 <-> 1691\n78 <-> 78\n79 <-> 9, 1396\n80 <-> 519, 636, 1091\n81 <-> 292\n82 <-> 286, 945\n83 <-> 403, 737\n84 <-> 806, 1435, 1893\n85 <-> 895, 955\n86 <-> 505\n87 <-> 1125\n88 <-> 88\n89 <-> 908\n90 <-> 1656\n91 <-> 91, 1476\n92 <-> 1524, 1699\n93 <-> 1298, 1994\n94 <-> 94, 1315\n95 <-> 1996\n96 <-> 575, 1837\n97 <-> 620\n98 <-> 1097, 1144\n99 <-> 266, 684\n100 <-> 1471\n101 <-> 262, 549\n102 <-> 209, 1585\n103 <-> 1333\n104 <-> 1794\n105 <-> 287, 787\n106 <-> 672, 891\n107 <-> 900, 979\n108 <-> 439, 1679, 1767\n109 <-> 431, 621, 861, 1297\n110 <-> 1043\n111 <-> 1355, 1479\n112 <-> 1195\n113 <-> 320, 1710\n114 <-> 159\n115 <-> 767, 984\n116 <-> 1918\n117 <-> 1445\n118 <-> 471, 599\n119 <-> 1599, 1625\n120 <-> 653, 1056\n121 <-> 1083, 1143\n122 <-> 1099, 1952\n123 <-> 474, 1491\n124 <-> 789, 1645\n125 <-> 825, 1862\n126 <-> 1913\n127 <-> 1312\n128 <-> 1016, 1704\n129 <-> 1516, 1892\n130 <-> 762, 1013, 1343\n131 <-> 188, 864\n132 <-> 380\n133 <-> 21, 340, 371\n134 <-> 772\n135 <-> 1062\n136 <-> 942\n137 <-> 652, 923, 1520, 1580, 1596\n138 <-> 1321, 1975\n139 <-> 139, 1664\n140 <-> 1061, 1357\n141 <-> 1589\n142 <-> 24, 1629, 1866\n143 <-> 903, 1466, 1798\n144 <-> 144, 792\n145 <-> 71\n146 <-> 575, 1401\n147 <-> 692\n148 <-> 204, 447, 1294\n149 <-> 407, 642\n150 <-> 982\n151 <-> 542, 995, 1027\n152 <-> 1586\n153 <-> 686\n154 <-> 289, 580\n155 <-> 259, 786, 859, 1487\n156 <-> 1096\n157 <-> 1939\n158 <-> 50, 341, 741, 1007, 1064\n159 <-> 114, 159, 325, 831\n160 <-> 1088\n161 <-> 425\n162 <-> 195\n163 <-> 479, 930\n164 <-> 1486, 1755\n165 <-> 228, 808\n166 <-> 63\n167 <-> 1673\n168 <-> 168\n169 <-> 243, 466\n170 <-> 1105\n171 <-> 570\n172 <-> 178, 526, 1318, 1364\n173 <-> 1126\n174 <-> 1330, 1383, 1754\n175 <-> 606, 1180\n176 <-> 51, 1685\n177 <-> 1872\n178 <-> 172\n179 <-> 13, 304\n180 <-> 424\n181 <-> 1092\n182 <-> 1329\n183 <-> 1195, 1861\n184 <-> 66, 1912\n185 <-> 185, 400\n186 <-> 1305, 1644\n187 <-> 648, 1931\n188 <-> 41, 54, 131, 1114\n189 <-> 206\n190 <-> 1794\n191 <-> 32, 1399\n192 <-> 466\n193 <-> 906, 1244, 1334\n194 <-> 1614\n195 <-> 162, 1660\n196 <-> 788\n197 <-> 1028, 1083\n198 <-> 1089, 1666\n199 <-> 879, 1609\n200 <-> 220\n201 <-> 201\n202 <-> 1431\n203 <-> 1503\n204 <-> 148\n205 <-> 205, 868\n206 <-> 189, 1561\n207 <-> 902, 1115\n208 <-> 712, 880\n209 <-> 102, 843, 1474\n210 <-> 707, 1411\n211 <-> 15, 845, 950\n212 <-> 360, 468, 1848\n213 <-> 1355\n214 <-> 888\n215 <-> 1270, 1604, 1718\n216 <-> 620, 1222\n217 <-> 1521, 1894\n218 <-> 227, 935, 1793, 1803\n219 <-> 566\n220 <-> 200, 1474\n221 <-> 540, 1836\n222 <-> 17, 487, 697\n223 <-> 1631\n224 <-> 742, 894\n225 <-> 1701\n226 <-> 387, 905, 1871\n227 <-> 218\n228 <-> 165, 865\n229 <-> 1150\n230 <-> 1286\n231 <-> 1789\n232 <-> 233, 1232, 1243, 1375\n233 <-> 232\n234 <-> 63, 447, 1165\n235 <-> 1992\n236 <-> 667, 1219\n237 <-> 1681, 1704, 1725\n238 <-> 1147, 1434\n239 <-> 517\n240 <-> 743\n241 <-> 1100, 1126\n242 <-> 1344\n243 <-> 169, 1582\n244 <-> 1185, 1815\n245 <-> 577, 1090\n246 <-> 476, 818, 1465\n247 <-> 592, 1928\n248 <-> 73, 1761\n249 <-> 1937\n250 <-> 956\n251 <-> 960\n252 <-> 746\n253 <-> 1545\n254 <-> 18\n255 <-> 255, 1362\n256 <-> 830, 1927\n257 <-> 461, 954\n258 <-> 489, 1338\n259 <-> 155, 712, 1802\n260 <-> 1121\n261 <-> 261\n262 <-> 101, 1009, 1020\n263 <-> 1791, 1968\n264 <-> 1816\n265 <-> 917, 1307\n266 <-> 99, 1041\n267 <-> 28, 459, 752, 1855\n268 <-> 296, 694\n269 <-> 1275\n270 <-> 1113\n271 <-> 750, 1174\n272 <-> 580\n273 <-> 1470\n274 <-> 1138\n275 <-> 512, 920\n276 <-> 863, 1570\n277 <-> 1830\n278 <-> 1674\n279 <-> 901, 934\n280 <-> 1004\n281 <-> 366, 545, 1161\n282 <-> 1744, 1878\n283 <-> 330\n284 <-> 1279\n285 <-> 873\n286 <-> 82, 357\n287 <-> 105\n288 <-> 63, 510\n289 <-> 154\n290 <-> 1456\n291 <-> 1154, 1156\n292 <-> 81, 753, 805, 1034\n293 <-> 1722\n294 <-> 1918\n295 <-> 315, 731\n296 <-> 268, 994, 1045\n297 <-> 1011\n298 <-> 862, 1020\n299 <-> 299, 442, 1452\n300 <-> 1393, 1438\n301 <-> 1351, 1507\n302 <-> 751, 1128\n303 <-> 1530, 1755\n304 <-> 179, 960\n305 <-> 418, 625\n306 <-> 1525, 1728\n307 <-> 817, 937, 1138\n308 <-> 50\n309 <-> 69, 309\n310 <-> 1307\n311 <-> 503, 981, 1025\n312 <-> 33, 856\n313 <-> 750, 1018\n314 <-> 47, 489, 726\n315 <-> 295, 1663\n316 <-> 565, 1900\n317 <-> 1105, 1384\n318 <-> 988\n319 <-> 830\n320 <-> 113, 320\n321 <-> 1500\n322 <-> 389, 1377\n323 <-> 791\n324 <-> 785\n325 <-> 159, 800, 1721\n326 <-> 326, 1316\n327 <-> 1097\n328 <-> 660, 1006\n329 <-> 590\n330 <-> 283, 330, 1223, 1502\n331 <-> 1436, 1471\n332 <-> 332\n333 <-> 1060\n334 <-> 824\n335 <-> 520, 678, 1302\n336 <-> 55, 410, 1092, 1093\n337 <-> 1369\n338 <-> 772, 1442\n339 <-> 49\n340 <-> 133, 1393\n341 <-> 158\n342 <-> 724\n343 <-> 71, 483, 970, 1446\n344 <-> 344, 617\n345 <-> 830, 1174\n346 <-> 1551\n347 <-> 1230, 1637, 1673, 1907\n348 <-> 804\n349 <-> 522, 1405\n350 <-> 574, 800, 1021\n351 <-> 1494\n352 <-> 352\n353 <-> 627, 1658\n354 <-> 1822\n355 <-> 527\n356 <-> 1108, 1175\n357 <-> 286, 435, 782, 961\n358 <-> 905\n359 <-> 691, 715\n360 <-> 212\n361 <-> 1071, 1788\n362 <-> 963, 1286, 1861\n363 <-> 612, 884, 1687\n364 <-> 451, 1668, 1805\n365 <-> 558, 604, 1593\n366 <-> 281\n367 <-> 1054, 1472\n368 <-> 7\n369 <-> 1686\n370 <-> 396, 1919\n371 <-> 133, 896\n372 <-> 1909\n373 <-> 59, 491, 1838\n374 <-> 561\n375 <-> 478, 1117, 1179\n376 <-> 1261, 1674\n377 <-> 822, 1188\n378 <-> 1536, 1551, 1635\n379 <-> 379\n380 <-> 132, 1866\n381 <-> 1935\n382 <-> 1980\n383 <-> 1139\n384 <-> 1502\n385 <-> 613\n386 <-> 781, 987\n387 <-> 226\n388 <-> 388\n389 <-> 322\n390 <-> 1009, 1198\n391 <-> 430, 1429\n392 <-> 1639\n393 <-> 1897\n394 <-> 1568, 1701\n395 <-> 928\n396 <-> 370, 885\n397 <-> 474, 622\n398 <-> 1174, 1280\n399 <-> 1449, 1661\n400 <-> 185, 1135\n401 <-> 839\n402 <-> 1167, 1977\n403 <-> 83, 935\n404 <-> 749\n405 <-> 405, 1775\n406 <-> 812, 913, 1719\n407 <-> 149, 875\n408 <-> 11\n409 <-> 635\n410 <-> 336, 653, 757\n411 <-> 1406\n412 <-> 520\n413 <-> 791, 1129\n414 <-> 581\n415 <-> 1, 769, 1250\n416 <-> 424, 903\n417 <-> 417, 1112\n418 <-> 305\n419 <-> 1414\n420 <-> 743, 1912\n421 <-> 1411\n422 <-> 56, 1365, 1895\n423 <-> 1228, 1874\n424 <-> 180, 416\n425 <-> 161, 548, 1428, 1817\n426 <-> 474\n427 <-> 477, 1982\n428 <-> 654, 1016, 1781, 1889\n429 <-> 1165, 1258\n430 <-> 391, 1648\n431 <-> 109\n432 <-> 432, 612, 1324\n433 <-> 1678\n434 <-> 988, 1272, 1322\n435 <-> 357\n436 <-> 1267, 1682\n437 <-> 975\n438 <-> 1632, 1865\n439 <-> 108, 439, 775\n440 <-> 5, 1510, 1635, 1744\n441 <-> 441\n442 <-> 299, 615, 801\n443 <-> 443\n444 <-> 794, 1042, 1868\n445 <-> 1427\n446 <-> 72, 1536\n447 <-> 148, 234\n448 <-> 469, 720, 1418\n449 <-> 449\n450 <-> 1628\n451 <-> 364\n452 <-> 6\n453 <-> 546\n454 <-> 894, 1001, 1137\n455 <-> 1235, 1764\n456 <-> 1232\n457 <-> 1864, 1946\n458 <-> 47\n459 <-> 267, 624, 1675\n460 <-> 509, 610, 1406, 1548\n461 <-> 257\n462 <-> 979, 1340, 1500, 1989\n463 <-> 551, 1483\n464 <-> 1291, 1397, 1543, 1786\n465 <-> 10, 1661\n466 <-> 169, 192, 553\n467 <-> 1394, 1987\n468 <-> 212\n469 <-> 448\n470 <-> 1536\n471 <-> 118, 477, 1962\n472 <-> 65, 983, 1763\n473 <-> 1988\n474 <-> 123, 397, 426\n475 <-> 475\n476 <-> 246, 718, 1177\n477 <-> 20, 427, 471\n478 <-> 375, 495\n479 <-> 163, 479, 1705, 1914\n480 <-> 744\n481 <-> 497, 1051\n482 <-> 1134\n483 <-> 343, 783\n484 <-> 556, 1360, 1735\n485 <-> 1464\n486 <-> 901, 1387\n487 <-> 222\n488 <-> 621, 1206, 1560\n489 <-> 258, 314\n490 <-> 1714\n491 <-> 373, 1038\n492 <-> 1127, 1848, 1960\n493 <-> 48\n494 <-> 638, 1948\n495 <-> 478, 1397\n496 <-> 1180\n497 <-> 13, 481, 1119\n498 <-> 1275\n499 <-> 1158\n500 <-> 988, 1058, 1758\n501 <-> 715, 1186, 1697\n502 <-> 23, 74, 605, 1514\n503 <-> 311\n504 <-> 550, 744, 807, 1378, 1636\n505 <-> 86, 1925\n506 <-> 506\n507 <-> 1944\n508 <-> 1130\n509 <-> 460\n510 <-> 288, 1254, 1434, 1997\n511 <-> 714, 884\n512 <-> 275\n513 <-> 1730\n514 <-> 2, 999\n515 <-> 1593\n516 <-> 615\n517 <-> 239, 816\n518 <-> 518, 765\n519 <-> 80, 1003\n520 <-> 335, 412, 915\n521 <-> 18, 569\n522 <-> 349\n523 <-> 523, 543\n524 <-> 524, 1270\n525 <-> 1285, 1850\n526 <-> 172\n527 <-> 355, 528, 1345\n528 <-> 527, 1229\n529 <-> 621, 1798\n530 <-> 993, 1608\n531 <-> 1, 647\n532 <-> 1474\n533 <-> 592\n534 <-> 1142\n535 <-> 1484\n536 <-> 554, 803\n537 <-> 1507\n538 <-> 588, 812\n539 <-> 539, 571\n540 <-> 221\n541 <-> 1261\n542 <-> 151, 1650\n543 <-> 523\n544 <-> 1672\n545 <-> 281, 1207, 1522\n546 <-> 453, 1169, 1809\n547 <-> 985, 1115, 1729\n548 <-> 425, 640\n549 <-> 101\n550 <-> 504, 1535, 1717\n551 <-> 45, 463, 1517, 1833\n552 <-> 734, 1667\n553 <-> 466, 1921\n554 <-> 536\n555 <-> 1401\n556 <-> 484, 910, 1930\n557 <-> 557\n558 <-> 365\n559 <-> 619, 630\n560 <-> 560\n561 <-> 374, 1961\n562 <-> 1863\n563 <-> 1238, 1733\n564 <-> 950\n565 <-> 316, 1157\n566 <-> 219, 824, 1274\n567 <-> 32\n568 <-> 1121\n569 <-> 521, 593, 1336, 1982\n570 <-> 171, 967\n571 <-> 539, 891\n572 <-> 785\n573 <-> 1009\n574 <-> 350\n575 <-> 96, 146, 1575\n576 <-> 601, 830, 1921\n577 <-> 245, 604\n578 <-> 1837\n579 <-> 1018, 1757\n580 <-> 154, 272, 888\n581 <-> 414, 1734\n582 <-> 1866\n583 <-> 971, 1936\n584 <-> 0, 1082\n585 <-> 1371\n586 <-> 586, 926\n587 <-> 672\n588 <-> 538\n589 <-> 1776\n590 <-> 329, 733\n591 <-> 897, 1511\n592 <-> 247, 533, 1313\n593 <-> 569, 929, 1551, 1994\n594 <-> 1249\n595 <-> 1728, 1797, 1872\n596 <-> 856, 1209\n597 <-> 1488, 1520\n598 <-> 16\n599 <-> 118\n600 <-> 1337, 1355, 1703\n601 <-> 576\n602 <-> 1615\n603 <-> 1264\n604 <-> 365, 577, 1356\n605 <-> 502\n606 <-> 175, 1491, 1584\n607 <-> 1777\n608 <-> 760, 1779\n609 <-> 737\n610 <-> 460\n611 <-> 611\n612 <-> 363, 432, 1963\n613 <-> 385, 708, 875, 1289, 1557, 1618\n614 <-> 1303\n615 <-> 442, 516\n616 <-> 1556, 1676\n617 <-> 344\n618 <-> 40, 641, 1124\n619 <-> 559, 1649\n620 <-> 97, 216\n621 <-> 109, 488, 529\n622 <-> 397, 886\n623 <-> 623, 716\n624 <-> 459\n625 <-> 305, 1126\n626 <-> 936, 1998\n627 <-> 353, 846, 1672\n628 <-> 628, 1734\n629 <-> 977, 1257, 1958\n630 <-> 559, 1559, 1977\n631 <-> 1744\n632 <-> 1008, 1620\n633 <-> 818, 1678\n634 <-> 46, 1259\n635 <-> 409, 663\n636 <-> 80, 1622\n637 <-> 1856\n638 <-> 494, 734, 1983\n639 <-> 639, 1402\n640 <-> 548, 1156, 1493\n641 <-> 618\n642 <-> 149\n643 <-> 643\n644 <-> 1233, 1512\n645 <-> 75, 645, 1909\n646 <-> 675\n647 <-> 531, 1346, 1352\n648 <-> 187, 1137, 1585\n649 <-> 1048\n650 <-> 1360\n651 <-> 1072, 1168\n652 <-> 137, 652, 771, 1282\n653 <-> 120, 410\n654 <-> 428, 1669\n655 <-> 1403\n656 <-> 1589\n657 <-> 1707\n658 <-> 24, 730\n659 <-> 1282, 1823\n660 <-> 328, 1453\n661 <-> 727\n662 <-> 1190, 1788, 1857\n663 <-> 635, 1622\n664 <-> 1471\n665 <-> 665\n666 <-> 1064, 1955\n667 <-> 236, 667\n668 <-> 1222, 1709\n669 <-> 766, 1048, 1127\n670 <-> 670, 1168\n671 <-> 1165\n672 <-> 106, 587, 1201, 1950\n673 <-> 949\n674 <-> 1623\n675 <-> 69, 646, 1489, 1976\n676 <-> 948\n677 <-> 1576, 1743\n678 <-> 335, 1084\n679 <-> 1081\n680 <-> 680, 747\n681 <-> 1015\n682 <-> 1062\n683 <-> 823, 1160\n684 <-> 99, 1415\n685 <-> 1695\n686 <-> 153, 1674\n687 <-> 873, 1959\n688 <-> 1688\n689 <-> 1125, 1439, 1614\n690 <-> 1787\n691 <-> 359, 691, 1945\n692 <-> 147, 1425, 1426\n693 <-> 1327, 1329\n694 <-> 268, 1263\n695 <-> 695, 1055, 1482\n696 <-> 780\n697 <-> 222, 1059\n698 <-> 1486\n699 <-> 799\n700 <-> 1598\n701 <-> 872, 1103\n702 <-> 1546, 1853\n703 <-> 703\n704 <-> 1533\n705 <-> 1822\n706 <-> 1894, 1969\n707 <-> 210, 729, 909\n708 <-> 613\n709 <-> 709\n710 <-> 1634\n711 <-> 1863\n712 <-> 208, 259\n713 <-> 713\n714 <-> 511, 1420\n715 <-> 359, 501\n716 <-> 623\n717 <-> 1104, 1193, 1431\n718 <-> 476\n719 <-> 27, 1861\n720 <-> 448, 720, 1374\n721 <-> 1632, 1721\n722 <-> 1944\n723 <-> 723\n724 <-> 342, 1782\n725 <-> 1485\n726 <-> 314, 1451\n727 <-> 661, 1348\n728 <-> 1569\n729 <-> 707\n730 <-> 658, 1656\n731 <-> 295\n732 <-> 59, 1368\n733 <-> 590, 733\n734 <-> 552, 638, 1908\n735 <-> 834\n736 <-> 983\n737 <-> 83, 609\n738 <-> 1189, 1339, 1647\n739 <-> 947, 1588\n740 <-> 35, 1883\n741 <-> 158, 784\n742 <-> 224\n743 <-> 240, 420\n744 <-> 480, 504\n745 <-> 1604\n746 <-> 252, 823, 1225\n747 <-> 680\n748 <-> 1366, 1474\n749 <-> 404, 1578\n750 <-> 31, 271, 313\n751 <-> 302\n752 <-> 267\n753 <-> 292, 1848\n754 <-> 1394, 1847\n755 <-> 755\n756 <-> 1168\n757 <-> 410, 1352, 1484, 1830\n758 <-> 852, 1037\n759 <-> 759\n760 <-> 608\n761 <-> 1060, 1272, 1943\n762 <-> 130, 1533\n763 <-> 774\n764 <-> 1270\n765 <-> 518\n766 <-> 669, 829, 1477\n767 <-> 115, 1933, 1935\n768 <-> 768\n769 <-> 415, 1475\n770 <-> 1087, 1370\n771 <-> 652\n772 <-> 134, 338, 1102, 1865\n773 <-> 1678, 1995\n774 <-> 763, 1712\n775 <-> 439, 981, 1505\n776 <-> 776, 1284\n777 <-> 915, 1192\n778 <-> 1268, 1269, 1371\n779 <-> 1101\n780 <-> 696, 894, 1144, 1339\n781 <-> 386, 1464\n782 <-> 357\n783 <-> 7, 483\n784 <-> 741, 1149\n785 <-> 324, 572, 817\n786 <-> 155, 1460\n787 <-> 105, 1368\n788 <-> 196, 1143, 1433\n789 <-> 124, 944, 1216, 1829\n790 <-> 46, 790, 1361\n791 <-> 323, 413, 1564, 1704, 1775\n792 <-> 144, 1079, 1839, 1860\n793 <-> 1248, 1880\n794 <-> 444, 1739, 1862\n795 <-> 861\n796 <-> 1997\n797 <-> 991, 1256, 1282\n798 <-> 1666\n799 <-> 699, 895\n800 <-> 76, 325, 350, 1808\n801 <-> 34, 442, 1153\n802 <-> 1035\n803 <-> 536, 1294\n804 <-> 348, 982\n805 <-> 292, 1382\n806 <-> 84, 1162\n807 <-> 504\n808 <-> 165, 808\n809 <-> 16, 1298, 1542\n810 <-> 1127\n811 <-> 1911\n812 <-> 406, 538\n813 <-> 8, 1733\n814 <-> 1140\n815 <-> 1400, 1596\n816 <-> 517, 1648\n817 <-> 57, 307, 785, 1596\n818 <-> 246, 633\n819 <-> 1955, 1956\n820 <-> 820, 1677\n821 <-> 1440, 1879\n822 <-> 377, 1032\n823 <-> 39, 683, 746, 1313\n824 <-> 334, 566\n825 <-> 125\n826 <-> 995\n827 <-> 827, 1328\n828 <-> 828, 1215\n829 <-> 766, 1549\n830 <-> 0, 256, 319, 345, 576\n831 <-> 159\n832 <-> 1320\n833 <-> 1772, 1891\n834 <-> 735, 834\n835 <-> 835, 908\n836 <-> 1975\n837 <-> 1491\n838 <-> 838\n839 <-> 401, 1068, 1236, 1780, 1849\n840 <-> 840, 1069\n841 <-> 1702\n842 <-> 1669\n843 <-> 209\n844 <-> 1940\n845 <-> 211, 898, 1508, 1708\n846 <-> 627\n847 <-> 1037, 1369\n848 <-> 1237\n849 <-> 1136, 1800, 1856\n850 <-> 1116\n851 <-> 1480, 1759\n852 <-> 758, 1515\n853 <-> 1708\n854 <-> 886\n855 <-> 1853\n856 <-> 312, 596, 1485, 1990\n857 <-> 931, 1697\n858 <-> 1912\n859 <-> 155\n860 <-> 922\n861 <-> 109, 795, 1971\n862 <-> 58, 298, 1392\n863 <-> 276, 1856\n864 <-> 131, 1690\n865 <-> 228, 933, 1913\n866 <-> 900, 1932\n867 <-> 1700\n868 <-> 205, 1363\n869 <-> 1954\n870 <-> 43\n871 <-> 1660\n872 <-> 701, 1226, 1252\n873 <-> 285, 687\n874 <-> 1181, 1915\n875 <-> 407, 613, 1353\n876 <-> 1218\n877 <-> 51, 937\n878 <-> 1733\n879 <-> 199, 1315\n880 <-> 208\n881 <-> 1708\n882 <-> 1108\n883 <-> 36, 1682\n884 <-> 363, 511\n885 <-> 396, 983\n886 <-> 622, 854\n887 <-> 887\n888 <-> 214, 580, 1272\n889 <-> 889\n890 <-> 1387\n891 <-> 106, 571\n892 <-> 1433\n893 <-> 1032, 1638\n894 <-> 224, 454, 780\n895 <-> 85, 799\n896 <-> 371\n897 <-> 591, 1721\n898 <-> 845\n899 <-> 899\n900 <-> 53, 107, 866\n901 <-> 279, 486, 1332, 1734\n902 <-> 207, 1043, 1164, 1491\n903 <-> 143, 416, 1015, 1722\n904 <-> 1178\n905 <-> 226, 358\n906 <-> 193, 1001\n907 <-> 964\n908 <-> 89, 835\n909 <-> 707, 963\n910 <-> 556, 910, 1495\n911 <-> 1203, 1613, 1954\n912 <-> 1348\n913 <-> 406\n914 <-> 75\n915 <-> 520, 777, 1463\n916 <-> 916, 1422, 1537, 1799\n917 <-> 265, 1487, 1824, 1920, 1931\n918 <-> 1731, 1828\n919 <-> 1116, 1794\n920 <-> 275, 920\n921 <-> 1218\n922 <-> 860, 1433\n923 <-> 137, 1455\n924 <-> 1656\n925 <-> 1628\n926 <-> 586, 1846\n927 <-> 1566\n928 <-> 395, 1499, 1807\n929 <-> 593, 1594\n930 <-> 163\n931 <-> 857\n932 <-> 1319\n933 <-> 865, 1113\n934 <-> 279\n935 <-> 218, 403\n936 <-> 626, 1211, 1242, 1444\n937 <-> 307, 877\n938 <-> 1145, 1389\n939 <-> 1396, 1981\n940 <-> 940, 1415\n941 <-> 1255, 1813\n942 <-> 136, 1206, 1373\n943 <-> 1540\n944 <-> 789\n945 <-> 82, 1500\n946 <-> 946\n947 <-> 739\n948 <-> 676, 1077, 1783, 1985\n949 <-> 673, 1271\n950 <-> 211, 564, 1371\n951 <-> 30, 996, 1546\n952 <-> 952\n953 <-> 1558\n954 <-> 257, 1011, 1365, 1583\n955 <-> 85, 1104\n956 <-> 250, 956\n957 <-> 1096\n958 <-> 1750\n959 <-> 1196, 1621\n960 <-> 251, 304\n961 <-> 357, 1727\n962 <-> 962\n963 <-> 362, 909\n964 <-> 907, 1227, 1999\n965 <-> 965\n966 <-> 1593\n967 <-> 570, 1159, 1544, 1960\n968 <-> 76, 1541\n969 <-> 43, 969\n970 <-> 343, 1834\n971 <-> 583\n972 <-> 1763, 1993\n973 <-> 1797, 1992\n974 <-> 1835\n975 <-> 437, 1790\n976 <-> 1245, 1279\n977 <-> 629, 1001\n978 <-> 1192\n979 <-> 107, 462, 1730\n980 <-> 1631\n981 <-> 311, 775, 1670\n982 <-> 150, 804, 1561, 1920\n983 <-> 472, 736, 885\n984 <-> 115, 1116\n985 <-> 547, 1829\n986 <-> 1174\n987 <-> 40, 386\n988 <-> 41, 318, 434, 500, 1038\n989 <-> 1002, 1922\n990 <-> 1010, 1395\n991 <-> 797\n992 <-> 1432\n993 <-> 530\n994 <-> 296\n995 <-> 151, 826, 1434\n996 <-> 951, 1122\n997 <-> 30, 1461\n998 <-> 1372\n999 <-> 514, 1283, 1386\n1000 <-> 1499\n1001 <-> 454, 906, 977\n1002 <-> 989\n1003 <-> 519, 1523, 1795\n1004 <-> 280, 1229, 1946\n1005 <-> 1534, 1538\n1006 <-> 328, 1117\n1007 <-> 158\n1008 <-> 632, 1869\n1009 <-> 262, 390, 573\n1010 <-> 990, 1805\n1011 <-> 297, 954\n1012 <-> 1689\n1013 <-> 130\n1014 <-> 1903\n1015 <-> 62, 681, 903, 1249, 1888\n1016 <-> 128, 428\n1017 <-> 1425, 1576, 1740\n1018 <-> 313, 579, 1845\n1019 <-> 1903\n1020 <-> 262, 298\n1021 <-> 350\n1022 <-> 1126\n1023 <-> 1371\n1024 <-> 1024, 1237\n1025 <-> 311\n1026 <-> 1431, 1437\n1027 <-> 151\n1028 <-> 197\n1029 <-> 1029\n1030 <-> 1194\n1031 <-> 1031\n1032 <-> 822, 893\n1033 <-> 1618, 1795\n1034 <-> 292, 1822\n1035 <-> 802, 1667\n1036 <-> 1214\n1037 <-> 758, 847\n1038 <-> 491, 988\n1039 <-> 36\n1040 <-> 1584, 1768\n1041 <-> 266\n1042 <-> 444, 1104, 1220\n1043 <-> 4, 110, 902\n1044 <-> 1633, 1682\n1045 <-> 296, 1652\n1046 <-> 1444\n1047 <-> 1047, 1255\n1048 <-> 649, 669\n1049 <-> 1882\n1050 <-> 1573\n1051 <-> 481\n1052 <-> 1149\n1053 <-> 1441\n1054 <-> 367, 1928\n1055 <-> 695\n1056 <-> 120\n1057 <-> 1410, 1684\n1058 <-> 500\n1059 <-> 697, 1944\n1060 <-> 333, 761, 1310, 1566\n1061 <-> 140, 1061, 1492\n1062 <-> 135, 682, 1062, 1377\n1063 <-> 1519, 1587, 1858\n1064 <-> 158, 666\n1065 <-> 1459, 1467, 1902\n1066 <-> 1969\n1067 <-> 1218\n1068 <-> 839\n1069 <-> 840, 1574\n1070 <-> 1814\n1071 <-> 361\n1072 <-> 651, 1218\n1073 <-> 31, 38\n1074 <-> 1241, 1468\n1075 <-> 1075\n1076 <-> 1490, 1634\n1077 <-> 948, 1111, 1279, 1832\n1078 <-> 1078\n1079 <-> 792\n1080 <-> 1327\n1081 <-> 679, 1653, 1719\n1082 <-> 584\n1083 <-> 121, 197, 1341\n1084 <-> 678\n1085 <-> 1576\n1086 <-> 1540, 1726\n1087 <-> 770, 1403\n1088 <-> 160, 1841\n1089 <-> 198, 1399, 1627\n1090 <-> 245, 1090\n1091 <-> 80, 1307\n1092 <-> 181, 336, 1211\n1093 <-> 336, 1851\n1094 <-> 1598, 1659\n1095 <-> 11, 1337\n1096 <-> 156, 957, 1103\n1097 <-> 98, 327\n1098 <-> 1235, 1261, 1567\n1099 <-> 122, 1348\n1100 <-> 241\n1101 <-> 779, 1253, 1526\n1102 <-> 772\n1103 <-> 701, 1096, 1978\n1104 <-> 717, 955, 1042\n1105 <-> 170, 317\n1106 <-> 1911\n1107 <-> 43\n1108 <-> 356, 882, 1123, 1991\n1109 <-> 1120\n1110 <-> 43, 56\n1111 <-> 1077, 1547\n1112 <-> 417\n1113 <-> 270, 933\n1114 <-> 188\n1115 <-> 207, 547\n1116 <-> 850, 919, 984, 1202\n1117 <-> 375, 1006, 1423, 1727\n1118 <-> 1855\n1119 <-> 497, 1736\n1120 <-> 1109, 1829\n1121 <-> 260, 568, 1151, 1155, 1644\n1122 <-> 996\n1123 <-> 1108\n1124 <-> 618, 1204, 1610, 1630, 1844\n1125 <-> 87, 689, 1228\n1126 <-> 173, 241, 625, 1022, 1461\n1127 <-> 492, 669, 810\n1128 <-> 302, 1128\n1129 <-> 413\n1130 <-> 508, 1852\n1131 <-> 1918\n1132 <-> 1413, 1527\n1133 <-> 1133, 1654\n1134 <-> 55, 482\n1135 <-> 400\n1136 <-> 849\n1137 <-> 454, 648, 1646\n1138 <-> 274, 307\n1139 <-> 383, 1951\n1140 <-> 814, 1140\n1141 <-> 1141\n1142 <-> 534, 1722\n1143 <-> 121, 788\n1144 <-> 98, 780\n1145 <-> 938, 1145\n1146 <-> 1242\n1147 <-> 238, 1680, 1699\n1148 <-> 1725\n1149 <-> 784, 1052\n1150 <-> 229, 1970\n1151 <-> 1121, 1379\n1152 <-> 1575\n1153 <-> 801, 1288\n1154 <-> 291\n1155 <-> 1121, 1785\n1156 <-> 291, 640\n1157 <-> 565, 1157\n1158 <-> 499, 1158\n1159 <-> 967\n1160 <-> 683\n1161 <-> 281\n1162 <-> 806, 1555, 1616\n1163 <-> 1268\n1164 <-> 902\n1165 <-> 234, 429, 671\n1166 <-> 1596\n1167 <-> 402\n1168 <-> 651, 670, 756\n1169 <-> 546\n1170 <-> 1170, 1478, 1877\n1171 <-> 1171\n1172 <-> 1224, 1856\n1173 <-> 1367, 1584, 1820\n1174 <-> 271, 345, 398, 986, 1295\n1175 <-> 356, 1890\n1176 <-> 1936, 1961\n1177 <-> 24, 476, 1742\n1178 <-> 904, 1280\n1179 <-> 375\n1180 <-> 175, 496\n1181 <-> 874\n1182 <-> 1182, 1184\n1183 <-> 64, 1736\n1184 <-> 1182\n1185 <-> 72, 244\n1186 <-> 501\n1187 <-> 72\n1188 <-> 377\n1189 <-> 738\n1190 <-> 662, 1987\n1191 <-> 52\n1192 <-> 777, 978\n1193 <-> 717\n1194 <-> 1030, 1272\n1195 <-> 112, 183\n1196 <-> 13, 959, 1308, 1691\n1197 <-> 1197\n1198 <-> 390, 1729\n1199 <-> 1242\n1200 <-> 1815\n1201 <-> 672, 1433\n1202 <-> 1116, 1691\n1203 <-> 911\n1204 <-> 1124, 1420\n1205 <-> 1428, 1942\n1206 <-> 488, 942, 1603, 1608\n1207 <-> 545\n1208 <-> 1583\n1209 <-> 596\n1210 <-> 1580\n1211 <-> 936, 1092\n1212 <-> 1317\n1213 <-> 1213\n1214 <-> 1036, 1391, 1395\n1215 <-> 828\n1216 <-> 789\n1217 <-> 1241\n1218 <-> 876, 921, 1067, 1072\n1219 <-> 236\n1220 <-> 1042\n1221 <-> 1531, 1669\n1222 <-> 216, 668, 1222, 1773, 1884\n1223 <-> 330, 1591\n1224 <-> 1172, 1260, 1677\n1225 <-> 746\n1226 <-> 872\n1227 <-> 62, 964\n1228 <-> 423, 1125\n1229 <-> 528, 1004, 1667\n1230 <-> 70, 347\n1231 <-> 1231, 1955\n1232 <-> 232, 456\n1233 <-> 17, 644\n1234 <-> 1239, 1737\n1235 <-> 455, 1098, 1910\n1236 <-> 52, 839\n1237 <-> 848, 1024\n1238 <-> 563, 1824\n1239 <-> 29, 1234\n1240 <-> 1240\n1241 <-> 1074, 1217\n1242 <-> 936, 1146, 1199\n1243 <-> 232, 1298\n1244 <-> 193\n1245 <-> 976, 1321\n1246 <-> 61, 1469\n1247 <-> 1247\n1248 <-> 793\n1249 <-> 594, 1015\n1250 <-> 415\n1251 <-> 1732\n1252 <-> 872, 1454, 1925\n1253 <-> 1101, 1315\n1254 <-> 510\n1255 <-> 941, 1047\n1256 <-> 797\n1257 <-> 629, 1663, 1881\n1258 <-> 429, 1740\n1259 <-> 634\n1260 <-> 1224\n1261 <-> 376, 541, 1098, 1624\n1262 <-> 1783\n1263 <-> 694\n1264 <-> 603, 1495, 1504\n1265 <-> 1868\n1266 <-> 1266, 1553\n1267 <-> 436, 1403\n1268 <-> 778, 1163, 1900\n1269 <-> 778\n1270 <-> 215, 524, 764\n1271 <-> 949, 1271, 1329\n1272 <-> 434, 761, 888, 1194\n1273 <-> 1565, 1605\n1274 <-> 566, 1612, 1634, 1878\n1275 <-> 269, 498, 1484, 1537\n1276 <-> 1876\n1277 <-> 1819\n1278 <-> 73\n1279 <-> 284, 976, 1077, 1657\n1280 <-> 398, 1178, 1755\n1281 <-> 1281\n1282 <-> 652, 659, 797, 1463\n1283 <-> 999\n1284 <-> 776\n1285 <-> 525\n1286 <-> 230, 362\n1287 <-> 1287, 1882\n1288 <-> 1153\n1289 <-> 613\n1290 <-> 1432\n1291 <-> 464\n1292 <-> 1605\n1293 <-> 1293\n1294 <-> 148, 803, 1443\n1295 <-> 1174\n1296 <-> 1296, 1592\n1297 <-> 109, 1792\n1298 <-> 93, 809, 1243\n1299 <-> 1299, 1975\n1300 <-> 1441\n1301 <-> 1793, 1856\n1302 <-> 335\n1303 <-> 614, 1307, 1941\n1304 <-> 1847\n1305 <-> 186\n1306 <-> 1306, 1765\n1307 <-> 265, 310, 1091, 1303\n1308 <-> 1196, 1916\n1309 <-> 1365, 1831\n1310 <-> 1060\n1311 <-> 1639\n1312 <-> 127, 1481, 1750\n1313 <-> 592, 823\n1314 <-> 1552, 1863\n1315 <-> 94, 879, 1253\n1316 <-> 326\n1317 <-> 1212, 1936\n1318 <-> 172, 1986\n1319 <-> 932, 1319\n1320 <-> 832, 1512\n1321 <-> 138, 1245\n1322 <-> 434, 1439\n1323 <-> 1323, 1715\n1324 <-> 432\n1325 <-> 1325, 1445\n1326 <-> 1572\n1327 <-> 693, 1080, 1494, 1641\n1328 <-> 827\n1329 <-> 182, 693, 1271\n1330 <-> 174\n1331 <-> 1331\n1332 <-> 901\n1333 <-> 103, 1333, 1404\n1334 <-> 193, 1615\n1335 <-> 1335, 1516\n1336 <-> 569\n1337 <-> 600, 1095\n1338 <-> 258\n1339 <-> 738, 780, 1662\n1340 <-> 462\n1341 <-> 1083\n1342 <-> 1662\n1343 <-> 130, 1615\n1344 <-> 242, 1344\n1345 <-> 527, 1600\n1346 <-> 647\n1347 <-> 48\n1348 <-> 727, 912, 1099, 1413\n1349 <-> 1506\n1350 <-> 1544\n1351 <-> 301, 1351\n1352 <-> 647, 757, 1683\n1353 <-> 875\n1354 <-> 1432, 1771\n1355 <-> 111, 213, 600, 1818, 1915\n1356 <-> 604, 1506\n1357 <-> 140\n1358 <-> 1990\n1359 <-> 1706\n1360 <-> 484, 650\n1361 <-> 790, 1633\n1362 <-> 255\n1363 <-> 868\n1364 <-> 7, 172, 1364\n1365 <-> 422, 954, 1309, 1586\n1366 <-> 748\n1367 <-> 1173\n1368 <-> 732, 787\n1369 <-> 337, 847, 1682\n1370 <-> 770\n1371 <-> 585, 778, 950, 1023, 1821\n1372 <-> 998, 1372, 1957\n1373 <-> 942\n1374 <-> 720\n1375 <-> 39, 232, 1871\n1376 <-> 1911\n1377 <-> 322, 1062\n1378 <-> 504\n1379 <-> 1151, 1939\n1380 <-> 1380\n1381 <-> 1787\n1382 <-> 805\n1383 <-> 174, 1636\n1384 <-> 317, 1384\n1385 <-> 1781\n1386 <-> 12, 999, 1843\n1387 <-> 486, 890\n1388 <-> 20\n1389 <-> 938\n1390 <-> 1390, 1623\n1391 <-> 1214\n1392 <-> 862, 1669\n1393 <-> 300, 340, 1817, 1841\n1394 <-> 467, 754\n1395 <-> 990, 1214, 1395\n1396 <-> 79, 939, 1881\n1397 <-> 464, 495, 1991\n1398 <-> 1398, 1642, 1686\n1399 <-> 191, 1089, 1643\n1400 <-> 815\n1401 <-> 146, 555\n1402 <-> 639\n1403 <-> 655, 1087, 1267, 1700\n1404 <-> 1333\n1405 <-> 349, 1638, 1746\n1406 <-> 411, 460, 1406\n1407 <-> 1407, 1459, 1688\n1408 <-> 1534\n1409 <-> 1731\n1410 <-> 22, 1057, 1734\n1411 <-> 210, 421, 1783\n1412 <-> 1631\n1413 <-> 1132, 1348\n1414 <-> 419, 1680\n1415 <-> 684, 940\n1416 <-> 68\n1417 <-> 1809\n1418 <-> 448\n1419 <-> 2\n1420 <-> 714, 1204\n1421 <-> 72, 1897\n1422 <-> 916\n1423 <-> 1117\n1424 <-> 1745, 1749\n1425 <-> 692, 1017\n1426 <-> 692, 1565\n1427 <-> 445, 1488, 1810\n1428 <-> 425, 1205, 1428\n1429 <-> 391, 1934\n1430 <-> 67, 1690\n1431 <-> 60, 202, 717, 1026, 1558\n1432 <-> 992, 1290, 1354\n1433 <-> 788, 892, 922, 1201\n1434 <-> 238, 510, 995\n1435 <-> 84\n1436 <-> 331\n1437 <-> 1026\n1438 <-> 300\n1439 <-> 689, 1322\n1440 <-> 821, 1573\n1441 <-> 1053, 1300, 1505, 1967\n1442 <-> 338\n1443 <-> 1294, 1696\n1444 <-> 936, 1046\n1445 <-> 117, 1325\n1446 <-> 343\n1447 <-> 1465\n1448 <-> 1520\n1449 <-> 399, 1674\n1450 <-> 1897\n1451 <-> 726, 1694\n1452 <-> 299\n1453 <-> 660, 1547\n1454 <-> 1252, 1551, 1762\n1455 <-> 923\n1456 <-> 290, 1456\n1457 <-> 1457\n1458 <-> 1626, 1645\n1459 <-> 1065, 1407\n1460 <-> 786\n1461 <-> 997, 1126, 1661\n1462 <-> 1596\n1463 <-> 915, 1282\n1464 <-> 485, 781\n1465 <-> 246, 1447, 1613, 1797\n1466 <-> 143\n1467 <-> 1065\n1468 <-> 1074, 1919\n1469 <-> 1246\n1470 <-> 74, 273\n1471 <-> 100, 331, 664, 1952\n1472 <-> 367\n1473 <-> 1473\n1474 <-> 209, 220, 532, 748, 1760\n1475 <-> 769, 1921\n1476 <-> 91, 1487\n1477 <-> 766, 1711, 1729, 1766\n1478 <-> 1170\n1479 <-> 111\n1480 <-> 851\n1481 <-> 1312, 1481\n1482 <-> 695\n1483 <-> 463, 1599\n1484 <-> 535, 757, 1275\n1485 <-> 725, 856, 1485\n1486 <-> 164, 698, 1756\n1487 <-> 155, 917, 1476, 1772\n1488 <-> 597, 1427\n1489 <-> 675\n1490 <-> 1076, 1672, 1922\n1491 <-> 123, 606, 837, 902\n1492 <-> 1061\n1493 <-> 640\n1494 <-> 351, 1327\n1495 <-> 910, 1264\n1496 <-> 1567\n1497 <-> 1497\n1498 <-> 1723\n1499 <-> 928, 1000, 1995\n1500 <-> 321, 462, 945\n1501 <-> 32\n1502 <-> 330, 384, 1707\n1503 <-> 203, 1687\n1504 <-> 1264\n1505 <-> 775, 1441, 1917\n1506 <-> 1349, 1356\n1507 <-> 301, 537\n1508 <-> 845\n1509 <-> 1981\n1510 <-> 440\n1511 <-> 591\n1512 <-> 644, 1320\n1513 <-> 1879\n1514 <-> 502, 1705\n1515 <-> 852\n1516 <-> 129, 1335, 1827\n1517 <-> 551, 1752\n1518 <-> 1697, 1714, 1873\n1519 <-> 1063, 1519, 1898\n1520 <-> 137, 597, 1448\n1521 <-> 217\n1522 <-> 545, 1522, 1801\n1523 <-> 1003\n1524 <-> 92\n1525 <-> 306\n1526 <-> 1101\n1527 <-> 1132\n1528 <-> 1691\n1529 <-> 1630\n1530 <-> 303\n1531 <-> 1221\n1532 <-> 1532\n1533 <-> 704, 762\n1534 <-> 1005, 1408, 1966\n1535 <-> 550, 1535\n1536 <-> 378, 446, 470\n1537 <-> 916, 1275\n1538 <-> 1005, 1619\n1539 <-> 49, 1720\n1540 <-> 943, 1086\n1541 <-> 968, 1777\n1542 <-> 809\n1543 <-> 464\n1544 <-> 967, 1350\n1545 <-> 253, 1618\n1546 <-> 702, 951\n1547 <-> 1111, 1453\n1548 <-> 460\n1549 <-> 829\n1550 <-> 1758\n1551 <-> 346, 378, 593, 1454\n1552 <-> 1314, 1552\n1553 <-> 1266\n1554 <-> 1554\n1555 <-> 1162, 1673\n1556 <-> 616\n1557 <-> 613\n1558 <-> 953, 1431\n1559 <-> 630, 1879\n1560 <-> 488, 1901\n1561 <-> 206, 982\n1562 <-> 9\n1563 <-> 1715\n1564 <-> 61, 791\n1565 <-> 1273, 1426\n1566 <-> 927, 1060\n1567 <-> 1098, 1496\n1568 <-> 394, 1568, 1999\n1569 <-> 728, 1591\n1570 <-> 276, 1602\n1571 <-> 1582\n1572 <-> 1326, 1572, 1741\n1573 <-> 48, 1050, 1440, 1817\n1574 <-> 1069\n1575 <-> 575, 1152, 1605, 1886\n1576 <-> 677, 1017, 1085\n1577 <-> 1953\n1578 <-> 749, 1578\n1579 <-> 1652, 1974, 1992\n1580 <-> 137, 1210\n1581 <-> 1696\n1582 <-> 243, 1571\n1583 <-> 954, 1208\n1584 <-> 606, 1040, 1173\n1585 <-> 102, 648\n1586 <-> 152, 1365\n1587 <-> 1063, 1972\n1588 <-> 739, 1990\n1589 <-> 141, 656, 1589\n1590 <-> 1590\n1591 <-> 1223, 1569\n1592 <-> 1296\n1593 <-> 365, 515, 966, 1698\n1594 <-> 929, 1826\n1595 <-> 1595\n1596 <-> 137, 815, 817, 1166, 1462\n1597 <-> 1969\n1598 <-> 700, 1094\n1599 <-> 119, 1483, 1671\n1600 <-> 1345\n1601 <-> 1601, 1731\n1602 <-> 1570\n1603 <-> 1206, 1984\n1604 <-> 215, 745\n1605 <-> 1273, 1292, 1575\n1606 <-> 1606\n1607 <-> 1649\n1608 <-> 530, 1206\n1609 <-> 199\n1610 <-> 1124, 1675\n1611 <-> 1611\n1612 <-> 1274, 1923\n1613 <-> 911, 1465\n1614 <-> 194, 689\n1615 <-> 602, 1334, 1343\n1616 <-> 52, 1162\n1617 <-> 1956\n1618 <-> 613, 1033, 1545\n1619 <-> 1538\n1620 <-> 632, 1620\n1621 <-> 959\n1622 <-> 636, 663\n1623 <-> 44, 674, 1390\n1624 <-> 1261\n1625 <-> 119\n1626 <-> 1458\n1627 <-> 1089, 1627\n1628 <-> 450, 925, 1628\n1629 <-> 142\n1630 <-> 1124, 1529\n1631 <-> 223, 980, 1412, 1788\n1632 <-> 438, 721\n1633 <-> 1044, 1361\n1634 <-> 710, 1076, 1274\n1635 <-> 378, 440\n1636 <-> 504, 1383, 1988\n1637 <-> 46, 347\n1638 <-> 893, 1405\n1639 <-> 26, 392, 1311, 1639\n1640 <-> 1865\n1641 <-> 1327\n1642 <-> 1398, 1723\n1643 <-> 1399\n1644 <-> 186, 1121, 1644, 1804\n1645 <-> 124, 1458\n1646 <-> 1137, 1859\n1647 <-> 738\n1648 <-> 430, 816\n1649 <-> 619, 1607\n1650 <-> 14, 542\n1651 <-> 6, 1970\n1652 <-> 1045, 1579\n1653 <-> 1081, 1742\n1654 <-> 1133\n1655 <-> 1924\n1656 <-> 90, 730, 924\n1657 <-> 1279\n1658 <-> 353, 1693\n1659 <-> 16, 1094\n1660 <-> 195, 871, 1991\n1661 <-> 399, 465, 1461\n1662 <-> 1339, 1342\n1663 <-> 315, 1257\n1664 <-> 139\n1665 <-> 1766\n1666 <-> 198, 798\n1667 <-> 552, 1035, 1229\n1668 <-> 364\n1669 <-> 654, 842, 1221, 1392\n1670 <-> 981\n1671 <-> 1599\n1672 <-> 544, 627, 1490\n1673 <-> 167, 347, 1555\n1674 <-> 68, 278, 376, 686, 1449\n1675 <-> 459, 1610, 1812\n1676 <-> 616, 1878\n1677 <-> 820, 1224\n1678 <-> 433, 633, 773\n1679 <-> 108\n1680 <-> 1147, 1414\n1681 <-> 237\n1682 <-> 436, 883, 1044, 1369\n1683 <-> 1352\n1684 <-> 1057, 1770\n1685 <-> 176\n1686 <-> 369, 1398, 1751, 1906\n1687 <-> 363, 1503\n1688 <-> 688, 1407, 1712\n1689 <-> 1012, 1689\n1690 <-> 864, 1430\n1691 <-> 77, 1196, 1202, 1528, 1973\n1692 <-> 1692\n1693 <-> 1658\n1694 <-> 1451, 1751, 1870\n1695 <-> 685, 1695\n1696 <-> 1443, 1581\n1697 <-> 501, 857, 1518\n1698 <-> 1593\n1699 <-> 92, 1147, 1720\n1700 <-> 867, 1403\n1701 <-> 225, 394\n1702 <-> 841, 1709\n1703 <-> 600\n1704 <-> 128, 237, 791, 1894\n1705 <-> 479, 1514\n1706 <-> 1359, 1706\n1707 <-> 657, 1502\n1708 <-> 845, 853, 881, 1910\n1709 <-> 668, 1702\n1710 <-> 113\n1711 <-> 1477\n1712 <-> 774, 1688\n1713 <-> 1806\n1714 <-> 490, 1518\n1715 <-> 1323, 1563\n1716 <-> 17\n1717 <-> 550\n1718 <-> 215\n1719 <-> 406, 1081\n1720 <-> 1539, 1699, 1935\n1721 <-> 325, 721, 897\n1722 <-> 293, 903, 1142\n1723 <-> 1498, 1642\n1724 <-> 1724, 1836\n1725 <-> 237, 1148\n1726 <-> 1086, 1835\n1727 <-> 961, 1117\n1728 <-> 306, 595\n1729 <-> 547, 1198, 1477\n1730 <-> 513, 979, 1753\n1731 <-> 918, 1409, 1601\n1732 <-> 1251, 1732\n1733 <-> 563, 813, 878, 1850\n1734 <-> 581, 628, 901, 1410\n1735 <-> 484\n1736 <-> 1119, 1183\n1737 <-> 1234, 1860\n1738 <-> 1738\n1739 <-> 794\n1740 <-> 1017, 1258\n1741 <-> 1572\n1742 <-> 1177, 1653\n1743 <-> 677\n1744 <-> 282, 440, 631\n1745 <-> 45, 1424\n1746 <-> 63, 1405\n1747 <-> 1992\n1748 <-> 1878\n1749 <-> 1424, 1753\n1750 <-> 958, 1312\n1751 <-> 1686, 1694\n1752 <-> 1517\n1753 <-> 1730, 1749\n1754 <-> 174\n1755 <-> 164, 303, 1280\n1756 <-> 1486, 1952, 1966\n1757 <-> 579\n1758 <-> 500, 1550\n1759 <-> 851, 1759\n1760 <-> 1474\n1761 <-> 248, 1761\n1762 <-> 1454\n1763 <-> 15, 472, 972\n1764 <-> 455\n1765 <-> 1306\n1766 <-> 1477, 1665\n1767 <-> 108\n1768 <-> 1040\n1769 <-> 1769\n1770 <-> 1684\n1771 <-> 1354, 1842\n1772 <-> 833, 1487\n1773 <-> 1222\n1774 <-> 1931\n1775 <-> 405, 791\n1776 <-> 589, 1776\n1777 <-> 607, 1541\n1778 <-> 1778\n1779 <-> 73, 608\n1780 <-> 839\n1781 <-> 428, 1385\n1782 <-> 724, 1805\n1783 <-> 948, 1262, 1411\n1784 <-> 1784\n1785 <-> 1155\n1786 <-> 464\n1787 <-> 690, 1381, 1825\n1788 <-> 361, 662, 1631\n1789 <-> 231, 1834\n1790 <-> 39, 975\n1791 <-> 263, 1988\n1792 <-> 1297\n1793 <-> 218, 1301\n1794 <-> 104, 190, 919\n1795 <-> 1003, 1033\n1796 <-> 1796, 1904\n1797 <-> 595, 973, 1465\n1798 <-> 143, 529\n1799 <-> 916\n1800 <-> 849, 1816\n1801 <-> 1522\n1802 <-> 259\n1803 <-> 218\n1804 <-> 1644\n1805 <-> 42, 364, 1010, 1782\n1806 <-> 1713, 1954\n1807 <-> 928\n1808 <-> 800\n1809 <-> 546, 1417, 1851\n1810 <-> 1427, 1974\n1811 <-> 1966\n1812 <-> 1675\n1813 <-> 941\n1814 <-> 30, 1070\n1815 <-> 244, 1200, 1965\n1816 <-> 264, 1800\n1817 <-> 425, 1393, 1573\n1818 <-> 1355\n1819 <-> 72, 1277\n1820 <-> 1173\n1821 <-> 1371\n1822 <-> 354, 705, 1034\n1823 <-> 659\n1824 <-> 917, 1238\n1825 <-> 1787, 1825\n1826 <-> 1594\n1827 <-> 1516\n1828 <-> 918\n1829 <-> 789, 985, 1120, 1980\n1830 <-> 277, 757\n1831 <-> 1309\n1832 <-> 1077\n1833 <-> 551\n1834 <-> 970, 1789\n1835 <-> 974, 1726, 1835\n1836 <-> 221, 1724\n1837 <-> 96, 578\n1838 <-> 373\n1839 <-> 792\n1840 <-> 1840\n1841 <-> 1088, 1393\n1842 <-> 1771, 1842\n1843 <-> 1386\n1844 <-> 1124\n1845 <-> 1018\n1846 <-> 926\n1847 <-> 754, 1304\n1848 <-> 212, 492, 753\n1849 <-> 839\n1850 <-> 525, 1733\n1851 <-> 1093, 1809\n1852 <-> 1130, 1852\n1853 <-> 702, 855\n1854 <-> 1946\n1855 <-> 267, 1118\n1856 <-> 637, 849, 863, 1172, 1301\n1857 <-> 74, 662\n1858 <-> 1063\n1859 <-> 1646\n1860 <-> 792, 1737\n1861 <-> 6, 183, 362, 719\n1862 <-> 125, 794\n1863 <-> 562, 711, 1314\n1864 <-> 457\n1865 <-> 438, 772, 1640\n1866 <-> 142, 380, 582\n1867 <-> 1867\n1868 <-> 444, 1265\n1869 <-> 1008\n1870 <-> 1694\n1871 <-> 226, 1375\n1872 <-> 177, 595\n1873 <-> 1518\n1874 <-> 423\n1875 <-> 1924\n1876 <-> 1276, 1876\n1877 <-> 1170, 1899\n1878 <-> 282, 1274, 1676, 1748\n1879 <-> 821, 1513, 1559\n1880 <-> 793, 1974\n1881 <-> 1257, 1396\n1882 <-> 1049, 1287\n1883 <-> 68, 740\n1884 <-> 1222\n1885 <-> 1934\n1886 <-> 1575\n1887 <-> 1966\n1888 <-> 1015\n1889 <-> 428\n1890 <-> 1175\n1891 <-> 833\n1892 <-> 129\n1893 <-> 84\n1894 <-> 217, 706, 1704\n1895 <-> 422\n1896 <-> 1896\n1897 <-> 393, 1421, 1450\n1898 <-> 1519\n1899 <-> 1877\n1900 <-> 316, 1268\n1901 <-> 1560, 1943\n1902 <-> 1065, 1959\n1903 <-> 1014, 1019, 1903\n1904 <-> 1796\n1905 <-> 1905\n1906 <-> 1686\n1907 <-> 347\n1908 <-> 734, 1938\n1909 <-> 372, 645\n1910 <-> 1235, 1708\n1911 <-> 811, 1106, 1376, 1911\n1912 <-> 184, 420, 858\n1913 <-> 126, 865\n1914 <-> 479\n1915 <-> 874, 1355, 1915\n1916 <-> 1308\n1917 <-> 1505\n1918 <-> 13, 116, 294, 1131, 1946\n1919 <-> 370, 1468\n1920 <-> 917, 982\n1921 <-> 553, 576, 1475\n1922 <-> 989, 1490\n1923 <-> 1612\n1924 <-> 1655, 1875, 1935\n1925 <-> 505, 1252\n1926 <-> 1926\n1927 <-> 256\n1928 <-> 19, 247, 1054\n1929 <-> 1975\n1930 <-> 556\n1931 <-> 187, 917, 1774\n1932 <-> 866\n1933 <-> 767, 1933\n1934 <-> 1429, 1885, 1934\n1935 <-> 381, 767, 1720, 1924\n1936 <-> 583, 1176, 1317, 1936\n1937 <-> 249, 1937\n1938 <-> 1908\n1939 <-> 157, 1379\n1940 <-> 844, 1940\n1941 <-> 1303\n1942 <-> 1205, 1979\n1943 <-> 761, 1901\n1944 <-> 507, 722, 1059\n1945 <-> 691\n1946 <-> 457, 1004, 1854, 1918\n1947 <-> 10\n1948 <-> 494\n1949 <-> 1949\n1950 <-> 672\n1951 <-> 1139, 1951\n1952 <-> 122, 1471, 1756\n1953 <-> 1577, 1953\n1954 <-> 869, 911, 1806\n1955 <-> 666, 819, 1231\n1956 <-> 819, 1617\n1957 <-> 1372\n1958 <-> 629\n1959 <-> 687, 1902\n1960 <-> 492, 967\n1961 <-> 561, 1176\n1962 <-> 471, 1964\n1963 <-> 612\n1964 <-> 1962\n1965 <-> 1815\n1966 <-> 1534, 1756, 1811, 1887\n1967 <-> 1441\n1968 <-> 263\n1969 <-> 706, 1066, 1597\n1970 <-> 1150, 1651\n1971 <-> 861\n1972 <-> 1587\n1973 <-> 1691\n1974 <-> 60, 1579, 1810, 1880\n1975 <-> 25, 138, 836, 1299, 1929\n1976 <-> 675\n1977 <-> 402, 630\n1978 <-> 1103\n1979 <-> 66, 1942\n1980 <-> 382, 1829\n1981 <-> 939, 1509\n1982 <-> 427, 569\n1983 <-> 638\n1984 <-> 1603\n1985 <-> 948\n1986 <-> 1318\n1987 <-> 467, 1190\n1988 <-> 473, 1636, 1791\n1989 <-> 462\n1990 <-> 856, 1358, 1588\n1991 <-> 1108, 1397, 1660\n1992 <-> 235, 973, 1579, 1747\n1993 <-> 972\n1994 <-> 42, 93, 593\n1995 <-> 773, 1499\n1996 <-> 95, 1996\n1997 <-> 510, 796\n1998 <-> 626\n1999 <-> 964, 1568"