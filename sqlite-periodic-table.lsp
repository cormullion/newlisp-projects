#!/usr/bin/env newlisp

(set 'elements
;No  Atomic-Weight Name Symbol MP BP  Density EarthCrust% DiscoveryYear Group IonizationEnergy (eV)
[text]1	1.0079	Hydrogen	H	-259	-253	0.09	0.14	1776	1	13.5984
2	4.0026	Helium	He	-272	-269	0	0	1895	18	24.5874
3	6.941	Lithium	Li	180	1347	0.53	0	1817	1	5.3917
4	9.0122	Beryllium	Be	1278	2970	1.85	0	1797	2	9.3227
5	10.811	Boron	B	2300	2550	2.34	0	1808	13	8.298
6	12.0107	Carbon	C	3500	4827	2.26	0.094	0	14	11.2603
7	14.0067	Nitrogen	N	-210	-196	1.25	0	1772	15	14.5341
8	15.9994	Oxygen	O	-218	-183	1.43	46.71	1774	16	13.6181
9	18.9984	Fluorine	F	-220	-188	1.7	0.029	1886	17	17.4228
10	20.1797	Neon	Ne	-249	-246	0	0	1898	18	21.5645
11	22.9897	Sodium	Na	98	883	0.97	2.75	1807	1	5.1391
12	24.305	Magnesium	Mg	639	1090	1.74	2.08	1755	2	7.6462
13	26.9815	Aluminum	Al	660	2467	2.7	8.07	1825	13	5.9858
14	28.0855	Silicon	Si	1410	2355	2.33	27.69	1824	14	8.1517
15	30.9738	Phosphorus	P	44	280	1.82	0.13	1669	15	10.4867
16	32.065	Sulfur	S	113	445	2.07	0.052	0	16	10.36
17	35.453	Chlorine	Cl	-101	-35	3.21	0.045	1774	17	12.9676
18	39.948	Argon	Ar	-189	-186	0	0	1894	18	15.7596
19	39.0983	Potassium	K	64	774	0.86	2.58	1807	1	4.3407
20	40.078	Calcium	Ca	839	1484	1.55	3.65	1808	2	6.1132
21	44.9559	Scandium	Sc	1539	2832	2.99	0	1879	3	6.5615
22	47.867	Titanium	Ti	1660	3287	4.54	0.62	1791	4	6.8281
23	50.9415	Vanadium	V	1890	3380	6.11	0	1830	5	6.7462
24	51.9961	Chromium	Cr	1857	2672	7.19	0.035	1797	6	6.7665
25	54.938	Manganese	Mn	1245	1962	7.43	0.09	1774	7	7.434
26	55.845	Iron	Fe	1535	2750	7.87	5.05	0	8	7.9024
27	58.9332	Cobalt	Co	1495	2870	8.9	0	1735	9	7.881
28	58.6934	Nickel	Ni	1453	2732	8.9	0.019	1751	10	7.6398
29	63.546	Copper	Cu	1083	2567	8.96	0	0	11	7.7264
30	65.39	Zinc	Zn	420	907	7.13	0	0	12	9.3942
31	69.723	Gallium	Ga	30	2403	5.91	0	1875	13	5.9993
32	72.64	Germanium	Ge	937	2830	5.32	0	1886	14	7.8994
33	74.9216	Arsenic	As	81	613	5.72	0	0	15	9.7886
34	78.96	Selenium	Se	217	685	4.79	0	1817	16	9.7524
35	79.904	Bromine	Br	-7	59	3.12	0	1826	17	11.8138
36	83.8	Krypton	Kr	-157	-153	0	0	1898	18	13.9996
37	85.4678	Rubidium	Rb	39	688	1.63	0	1861	1	4.1771
38	87.62	Strontium	Sr	769	1384	2.54	0	1790	2	5.6949
39	88.9059	Yttrium	Y	1523	3337	4.47	0	1794	3	6.2173
40	91.224	Zirconium	Zr	1852	4377	6.51	0.025	1789	4	6.6339
41	92.9064	Niobium	Nb	2468	4927	8.57	0	1801	5	6.7589
42	95.94	Molybdenum	Mo	2617	4612	10.22	0	1781	6	7.0924
43	98	Technetium	Tc	2200	4877	11.5	0	1937	7	7.28
44	101.07	Ruthenium	Ru	2250	3900	12.37	0	1844	8	7.3605
45	102.9055	Rhodium	Rh	1966	3727	12.41	0	1803	9	7.4589
46	106.42	Palladium	Pd	1552	2927	12.02	0	1803	10	8.3369
47	107.8682	Silver	Ag	962	2212	10.5	0	0	11	7.5762
48	112.411	Cadmium	Cd	321	765	8.65	0	1817	12	8.9938
49	114.818	Indium	In	157	2000	7.31	0	1863	13	5.7864
50	118.71	Tin	Sn	232	2270	7.31	0	0	14	7.3439
51	121.76	Antimony	Sb	630	1750	6.68	0	0	15	8.6084
52	127.6	Tellurium	Te	449	990	6.24	0	1783	16	9.0096
53	126.9045	Iodine	I	114	184	4.93	0	1811	17	10.4513
54	131.293	Xenon	Xe	-112	-108	0	0	1898	18	12.1298
55	132.9055	Cesium	Cs	29	678	1.87	0	1860	1	3.8939
56	137.327	Barium	Ba	725	1140	3.59	0.05	1808	2	5.2117
57	138.9055	Lanthanum	La	920	3469	6.15	0	1839	3	5.5769
58	140.116	Cerium	Ce	795	3257	6.77	0	1803	101	5.5387
59	140.9077	Praseodymium	Pr	935	3127	6.77	0	1885	101	5.473
60	144.24	Neodymium	Nd	1010	3127	7.01	0	1885	101	5.525
61	145	Promethium	Pm	1100	3000	7.3	0	1945	101	5.582
62	150.36	Samarium	Sm	1072	1900	7.52	0	1879	101	5.6437
63	151.964	Europium	Eu	822	1597	5.24	0	1901	101	5.6704
64	157.25	Gadolinium	Gd	1311	3233	7.9	0	1880	101	6.1501
65	158.9253	Terbium	Tb	1360	3041	8.23	0	1843	101	5.8638
66	162.5	Dysprosium	Dy	1412	2562	8.55	0	1886	101	5.9389
67	164.9303	Holmium	Ho	1470	2720	8.8	0	1867	101	6.0215
68	167.259	Erbium	Er	1522	2510	9.07	0	1842	101	6.1077
69	168.9342	Thulium	Tm	1545	1727	9.32	0	1879	101	6.1843
70	173.04	Ytterbium	Yb	824	1466	6.9	0	1878	101	6.2542
71	174.967	Lutetium	Lu	1656	3315	9.84	0	1907	101	5.4259
72	178.49	Hafnium	Hf	2150	5400	13.31	0	1923	4	6.8251
73	180.9479	Tantalum	Ta	2996	5425	16.65	0	1802	5	7.5496
74	183.84	Tungsten	W	3410	5660	19.35	0	1783	6	7.864
75	186.207	Rhenium	Re	3180	5627	21.04	0	1925	7	7.8335
76	190.23	Osmium	Os	3045	5027	22.6	0	1803	8	8.4382
77	192.217	Iridium	Ir	2410	4527	22.4	0	1803	9	8.967
78	195.078	Platinum	Pt	1772	3827	21.45	0	1735	10	8.9587
79	196.9665	Gold	Au	1064	2807	19.32	0	0	11	9.2255
80	200.59	Mercury	Hg	-39	357	13.55	0	0	12	10.4375
81	204.3833	Thallium	Tl	303	1457	11.85	0	1861	13	6.1082
82	207.2	Lead	Pb	327	1740	11.35	0	0	14	7.4167
83	208.9804	Bismuth	Bi	271	1560	9.75	0	0	15	7.2856
84	209	Polonium	Po	254	962	9.3	0	1898	16	8.417
85	210	Astatine	At	302	337	0	0	1940	17	9.3
86	222	Radon	Rn	-71	-62	0	0	1900	18	10.7485
87	223	Francium	Fr	27	677	0	0	1939	1	4.0727
88	226	Radium	Ra	700	1737	5.5	0	1898	2	5.2784
89	227	Actinium	Ac	1050	3200	10.07	0	1899	3	5.17
90	232.0381	Thorium	Th	1750	4790	11.72	0	1829	102	6.3067
91	231.0359	Protactinium	Pa	1568	0	15.4	0	1913	102	5.89
92	238.0289	Uranium	U	1132	3818	18.95	0	1789	102	6.1941
93	237	Neptunium	Np	640	3902	20.2	0	1940	102	6.2657
94	244	Plutonium	Pu	640	3235	19.84	0	1940	102	6.0262
95	243	Americium	Am	994	2607	13.67	0	1944	102	5.9738
96	247	Curium	Cm	1340	0	13.5	0	1944	102	5.9915
97	247	Berkelium	Bk	986	0	14.78	0	1949	102	6.1979
98	251	Californium	Cf	900	0	15.1	0	1950	102	6.2817
99	252	Einsteinium	Es	860	0	0	0	1952	102	6.42
100	257	Fermium	Fm	1527	0	0	0	1952	102	6.5
101	258	Mendelevium	Md	0	0	0	0	1955	102	6.58
102	259	Nobelium	No	827	0	0	0	1958	102	6.65
103	262	Lawrencium	Lr	1627	0	0	0	1961	102	4.9
104	261	Rutherfordium	Rf	0	0	0	0	1964	4	0
105	262	Dubnium	Db	0	0	0	0	1967	5	0
106	266	Seaborgium	Sg	0	0	0	0	1974	6	0
107	264	Bohrium	Bh	0	0	0	0	1981	7	0
108	277	Hassium	Hs	0	0	0	0	1984	8	0
109	268	Meitnerium	Mt	0	0	0	0	1982	9	0[/text])

(module "sqlite3.lsp")

(change-dir "/tmp")

; open or create a database
(if (sql3:open "periodic_table.db") 
		(println "database opened/created")
		(println "problem: " (sql3:error)))

(set 'column-def "number INTEGER, atomic_weight FLOAT, element TEXT, symbol TEXT, mp FLOAT, bp FLOAT, density FLOAT, earth_crust FLOAT, discovered INTEGER, egroup INTEGER, ionization FLOAT")

(define (create-table)
;;; create a table called t1
(println (string "create table t1 (" column-def ");"))
(if (sql3:sql (string "create table t1 (" column-def ")"))
		(println "created table  ... Ok")
		(println "problem " (sql3:error))))

(define (init-table)
;;; fill the table with data
	(dolist (e (parse elements "\n" 0))
	(set 'line (parse e))
	(if (sql3:sql 
		(format "insert into t1 values ('%d','%f','%s','%s','%f','%f','%f','%f', '%d', '%d', '%f');" 
		(int (line 0))
		(float (line 1))
		(line 2) 
		(line 3) 
		(float (line 4)) 
		(float (line 5))
		(float (line 6))
		(float (line 7))
		(int (line 8))
		(int (line 9))
		(float (line 10))))
		; success
		(println "inserted element " e )
		; failure
		(println (sql3:error) ":" "problem inserting " e))))

; if there's not "t1" table, create one and fill it
(unless (find "t1" (sql3:tables))
	(and
		(create-table)
		(init-table)))

(pretty-print 80)

(define (query sql-text)
	; run a query
	(set 'sqlarray (sql3:sql sql-text))
	(if sqlarray
		(begin
			(println "\n\nquery: " sql-text)
			(println "")
			(map println sqlarray))
		(println (sql3:error) " problem with select")))

(query "select symbol, element, discovered  from t1 where egroup = 18") ; the noble gases
(query "select element from t1  where earth_crust = 0.0")
(query "select * from t1  order by discovered")
(query "select element,earth_crust, discovered from t1 where discovered < 1900 and earth_crust > 2 order by discovered")
(query "select element,symbol,atomic_weight from t1  where symbol like 'A%' order by element")
(query "select * from t1  where mp > 3000") 
(query "select * from t1  where bp < mp ")
(query "select * from t1  order by random() limit 10 ")
(query "select element,atomic_weight from t1 where element like '%en' order by random() limit 30")
(query "select * from t1 order by symbol  limit 1 offset 2")
(query "select name,bp from t1 where bp > 5000 order by bp")
(query "select * from t1  where name like '%en' and bp > 10")
(query "select * from t1  where name like '%en'")
(query "select * from t1  where element like '%ium'")
