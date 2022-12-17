-------------------------
--/* 1.- PARADEROS */----
-------------------------

-----------------------------------------------------
--/* 1.1.- Paraderos de un determinado recorrido */--
-----------------------------------------------------

-- Ruta 422 Recorrido de ida

CREATE TABLE paradas_422 AS 
(SELECT stop_id, stop_name, stop_lat, stop_lon FROM gtfs.stops WHERE stop_id IN (
  SELECT DISTINCT stop_id FROM gtfs.stop_times WHERE trip_id IN (
    SELECT trip_id FROM gtfs.trips WHERE route_id = '422' AND 
	  direction_id = '0' AND
  		shape_id = '422I')));

-- Ruta G08 recorrida ida

CREATE TABLE paradas_g08 AS
(SELECT stop_id, stop_name, stop_lat, stop_lon FROM gtfs.stops WHERE stop_id IN (
  SELECT DISTINCT stop_id FROM gtfs.stop_times WHERE trip_id IN (
    SELECT trip_id FROM gtfs.trips WHERE route_id = 'G08' AND 
	  direction_id = '0' AND
  		shape_id = 'G08I')));
	
-- Ruta I09 recorrida ida

CREATE TABLE paradas_i09 AS
(SELECT stop_id, stop_name, stop_lat, stop_lon FROM gtfs.stops WHERE stop_id IN (
  SELECT DISTINCT stop_id FROM gtfs.stop_times WHERE trip_id IN (
    SELECT trip_id FROM gtfs.trips WHERE route_id = 'I09' AND 
	  direction_id = '0' AND
  		shape_id = 'I09I')));
		
-- Ruta C01 recorrida ida

CREATE TABLE paradas_c01 AS
(SELECT stop_id, stop_name, stop_lat, stop_lon FROM gtfs.stops WHERE stop_id IN (
  SELECT DISTINCT stop_id FROM gtfs.stop_times WHERE trip_id IN (
    SELECT trip_id FROM gtfs.trips WHERE route_id = 'C01' AND 
	  direction_id = '0' AND
  		shape_id = 'C01I')));		

-- Se añaden columnas de geometría
ALTER TABLE paradas_422 ADD COLUMN geom geometry(Point, 4326);
ALTER TABLE paradas_g08 ADD COLUMN geom geometry(Point, 4326);
ALTER TABLE paradas_i09 ADD COLUMN geom geometry(Point, 4326);
ALTER TABLE paradas_c01 ADD COLUMN geom geometry(Point, 4326);

-- Se pobla el campo geom
UPDATE paradas_422 SET geom = ST_MakePoint(stop_lon::float, stop_lat::float);
UPDATE paradas_g08 SET geom = ST_MakePoint(stop_lon::float, stop_lat::float);
UPDATE paradas_i09 SET geom = ST_MakePoint(stop_lon::float, stop_lat::float);
UPDATE paradas_c01 SET geom = ST_MakePoint(stop_lon::float, stop_lat::float);

---------------------------------------------------
--/* 1.2.- Paradas de un determinado recorrido */--
---------------------------------------------------

CREATE TABLE paradas_ruta_422 AS
(SELECT *, ST_SetSRID((ST_MakePoint(shape_pt_lon::float, shape_pt_lat::float)), 4326) as geom 
FROM gtfs.shapes
WHERE shape_id = '422I')

CREATE TABLE paradas_ruta_g08 AS
(SELECT *, ST_SetSRID((ST_MakePoint(shape_pt_lon::float, shape_pt_lat::float)), 4326) as geom 
FROM gtfs.shapes
WHERE shape_id = 'G08I')

CREATE TABLE paradas_ruta_i09 AS
(SELECT *, ST_SetSRID((ST_MakePoint(shape_pt_lon::float, shape_pt_lat::float)), 4326) as geom 
FROM gtfs.shapes
WHERE shape_id = 'I09I')

CREATE TABLE paradas_ruta_c01 AS
(SELECT *, ST_SetSRID((ST_MakePoint(shape_pt_lon::float, shape_pt_lat::float)), 4326) as geom 
FROM gtfs.shapes
WHERE shape_id = 'C01I')

-------------------------------------------------
--/* 1-3.- Ruta del recorrido como polilinea */--
-------------------------------------------------

CREATE TABLE ruta_422 as
(SELECT ST_MakeLine(paradas.geom ORDER BY shape_pt_sequence::int) AS geom
FROM paradas_ruta_422 AS paradas);

CREATE TABLE ruta_g08 as
(SELECT ST_MakeLine(paradas.geom ORDER BY shape_pt_sequence::int) AS geom
FROM paradas_ruta_g08 AS paradas);

CREATE TABLE ruta_i09 as
(SELECT ST_MakeLine(paradas.geom ORDER BY shape_pt_sequence::int) AS geom
FROM paradas_ruta_i09 AS paradas);

CREATE TABLE ruta_c01 as
(SELECT ST_MakeLine(paradas.geom ORDER BY shape_pt_sequence::int) AS geom
FROM paradas_ruta_c01 AS paradas);

---------------------------------------------------------------------------
--/* 1.4.- Se crea tabla con el número de rutas en todos los paraderos */--
---------------------------------------------------------------------------

CREATE TABLE paraderos_n_rutas AS
(SELECT st.stop_id, COUNT(DISTINCT r.route_short_name) AS n_paraderos
    FROM gtfs.stop_times st
    INNER JOIN gtfs.trips t ON t.trip_id = st.trip_id
    INNER JOIN gtfs.routes r ON r.route_id = t.route_id
	GROUP BY st.stop_id);
	


-------------------------------------------------------
--/* Número de malls y supermercados por paradero  */--
-------------------------------------------------------

-- Malls --

CREATE TABLE paraderos_n_malls AS
(SELECT paradas.stop_id, COUNT(malls.geom) as n_mall
FROM gtfs.stops AS paradas
LEFT JOIN malls 
ON ST_Intersects(ST_Buffer(ST_Transform(paradas.geom, 32719),500), 
				 ST_Transform(malls.geom, 32719))
GROUP BY paradas.stop_id);


-- Universidades --
CREATE TABLE paraderos_n_ues AS
(SELECT paradas.stop_id, COUNT(ues.geom) as n_ues
FROM gtfs.stops AS paradas
LEFT JOIN universidades as ues 
ON ST_Intersects(ST_Buffer(ST_Transform(paradas.geom, 32719),500), 
				 ST_Transform(ues.geom, 32719))
GROUP BY paradas.stop_id);

----------------------------------------------------------------------------
--/* 1.5 Creación tabla final de paraderos de un determinado recorrido  */--
----------------------------------------------------------------------------

-- 422

CREATE TABLE ahp_paraderos_422 AS
(SELECT mall_rutas.*, ues.n_ues
FROM
	(SELECT par_rut.*, malls.n_mall
	FROM
		(SELECT paraderos.*, rutas.n_paraderos AS n_rutas
		FROM paradas_422 AS paraderos
		LEFT JOIN paraderos_n_rutas AS rutas
		ON paraderos.stop_id = rutas.stop_id) AS par_rut
	LEFT JOIN paraderos_n_malls AS malls
	ON malls.stop_id = par_rut.stop_id) AS mall_rutas
LEFT JOIN paraderos_n_ues as ues
ON ues.stop_id = mall_rutas.stop_id);

-- G08

CREATE TABLE ahp_paraderos_g08 AS
(SELECT mall_rutas.*, ues.n_ues
FROM
	(SELECT par_rut.*, malls.n_mall
	FROM
		(SELECT paraderos.*, rutas.n_paraderos AS n_rutas
		FROM paradas_g08 AS paraderos
		LEFT JOIN paraderos_n_rutas AS rutas
		ON paraderos.stop_id = rutas.stop_id) AS par_rut
	LEFT JOIN paraderos_n_malls AS malls
	ON malls.stop_id = par_rut.stop_id) AS mall_rutas
LEFT JOIN paraderos_n_ues as ues
ON ues.stop_id = mall_rutas.stop_id);

-- I09

CREATE TABLE ahp_paraderos_i09 AS
(SELECT mall_rutas.*, ues.n_ues
FROM
	(SELECT par_rut.*, malls.n_mall
	FROM
		(SELECT paraderos.*, rutas.n_paraderos AS n_rutas
		FROM paradas_i09 AS paraderos
		LEFT JOIN paraderos_n_rutas AS rutas
		ON paraderos.stop_id = rutas.stop_id) AS par_rut
	LEFT JOIN paraderos_n_malls AS malls
	ON malls.stop_id = par_rut.stop_id) AS mall_rutas
LEFT JOIN paraderos_n_ues as ues
ON ues.stop_id = mall_rutas.stop_id);

-- C01

CREATE TABLE ahp_paraderos_c01 AS
(SELECT mall_rutas.*, ues.n_ues
FROM
	(SELECT par_rut.*, malls.n_mall
	FROM
		(SELECT paraderos.*, rutas.n_paraderos AS n_rutas
		FROM paradas_c01 AS paraderos
		LEFT JOIN paraderos_n_rutas AS rutas
		ON paraderos.stop_id = rutas.stop_id) AS par_rut
	LEFT JOIN paraderos_n_malls AS malls
	ON malls.stop_id = par_rut.stop_id) AS mall_rutas
LEFT JOIN paraderos_n_ues as ues
ON ues.stop_id = mall_rutas.stop_id);

-- Indicador de accesibilidad
/*
n_rutas: 0 - 3; 4 - 6; 7 - 11; 12 -19
		  25	 50		75		100
		  ponderacion: 60
malls: 0; 1 - 5
	   50	100
	   ponderación: 20
ues: 0; 1 - 19
	50		100
	ponderacion: 20

*/

-- Nuevas columnas

ALTER TABLE ahp_paraderos_422
ADD COLUMN valor_eq_rutas INT,
ADD COLUMN valor_eq_mall INT,
ADD COLUMN valor_eq_u INT,
ADD COLUMN eq_pond_rutas INT,
ADD COLUMN eq_pond_mall INT,
ADD COLUMN eq_pond_u INT,
ADD COLUMN weight FLOAT;

ALTER TABLE ahp_paraderos_g08
ADD COLUMN valor_eq_rutas INT,
ADD COLUMN valor_eq_mall INT,
ADD COLUMN valor_eq_u INT,
ADD COLUMN eq_pond_rutas INT,
ADD COLUMN eq_pond_mall INT,
ADD COLUMN eq_pond_u INT,
ADD COLUMN weight FLOAT;

ALTER TABLE ahp_paraderos_i09
ADD COLUMN valor_eq_rutas INT,
ADD COLUMN valor_eq_mall INT,
ADD COLUMN valor_eq_u INT,
ADD COLUMN eq_pond_rutas INT,
ADD COLUMN eq_pond_mall INT,
ADD COLUMN eq_pond_u INT,
ADD COLUMN weight FLOAT;

ALTER TABLE ahp_paraderos_c01
ADD COLUMN valor_eq_rutas INT,
ADD COLUMN valor_eq_mall INT,
ADD COLUMN valor_eq_u INT,
ADD COLUMN eq_pond_rutas INT,
ADD COLUMN eq_pond_mall INT,
ADD COLUMN eq_pond_u INT,
ADD COLUMN weight FLOAT;

-- Se poblan las nuevas columnas

-- 422

UPDATE ahp_paraderos_422
SET
 valor_eq_rutas = CASE WHEN n_rutas BETWEEN 0 AND 3 THEN 25 
 				 	   WHEN n_rutas BETWEEN 4 AND 6 THEN 50
				 	   WHEN n_rutas BETWEEN 7 AND 11 THEN 75
				 	   ELSE 100 END,
 valor_eq_mall =  CASE WHEN n_mall = 0 THEN 50
 					   ELSE 100 END,
 valor_eq_u = 	  CASE WHEN n_mall = 0 THEN 50
 					   ELSE 100 END;


UPDATE ahp_paraderos_422
SET
 eq_pond_rutas = 60 * valor_eq_rutas,
 eq_pond_mall = 20 * valor_eq_mall,
 eq_pond_u = 20 * valor_eq_u;


UPDATE ahp_paraderos_422
SET
 weight = (eq_pond_rutas + eq_pond_mall + eq_pond_u)/100;
 
-- G08 
 
UPDATE ahp_paraderos_g08
SET
 valor_eq_rutas = CASE WHEN n_rutas BETWEEN 0 AND 3 THEN 25 
 				 	   WHEN n_rutas BETWEEN 4 AND 6 THEN 50
				 	   WHEN n_rutas BETWEEN 7 AND 11 THEN 75
				 	   ELSE 100 END,
 valor_eq_mall =  CASE WHEN n_mall = 0 THEN 50
 					   ELSE 100 END,
 valor_eq_u = 	  CASE WHEN n_mall = 0 THEN 50
 					   ELSE 100 END; 
 
UPDATE ahp_paraderos_g08
SET
 eq_pond_rutas = 60 * valor_eq_rutas,
 eq_pond_mall = 20 * valor_eq_mall,
 eq_pond_u = 20 * valor_eq_u;

UPDATE ahp_paraderos_g08
SET
 weight = (eq_pond_rutas + eq_pond_mall + eq_pond_u)/100; 

-- I09

UPDATE ahp_paraderos_i09
SET
 valor_eq_rutas = CASE WHEN n_rutas BETWEEN 0 AND 3 THEN 25 
 				 	   WHEN n_rutas BETWEEN 4 AND 6 THEN 50
				 	   WHEN n_rutas BETWEEN 7 AND 11 THEN 75
				 	   ELSE 100 END,
 valor_eq_mall =  CASE WHEN n_mall = 0 THEN 50
 					   ELSE 100 END,
 valor_eq_u = 	  CASE WHEN n_mall = 0 THEN 50
 					   ELSE 100 END; 
 
UPDATE ahp_paraderos_i09
SET
 eq_pond_rutas = 60 * valor_eq_rutas,
 eq_pond_mall = 20 * valor_eq_mall,
 eq_pond_u = 20 * valor_eq_u;

UPDATE ahp_paraderos_i09
SET
 weight = (eq_pond_rutas + eq_pond_mall + eq_pond_u)/100; 

-- c01

UPDATE ahp_paraderos_c01
SET
 valor_eq_rutas = CASE WHEN n_rutas BETWEEN 0 AND 3 THEN 25 
 				 	   WHEN n_rutas BETWEEN 4 AND 6 THEN 50
				 	   WHEN n_rutas BETWEEN 7 AND 11 THEN 75
				 	   ELSE 100 END,
 valor_eq_mall =  CASE WHEN n_mall = 0 THEN 50
 					   ELSE 100 END,
 valor_eq_u = 	  CASE WHEN n_mall = 0 THEN 50
 					   ELSE 100 END; 
 
UPDATE ahp_paraderos_c01
SET
 eq_pond_rutas = 60 * valor_eq_rutas,
 eq_pond_mall = 20 * valor_eq_mall,
 eq_pond_u = 20 * valor_eq_u;

UPDATE ahp_paraderos_c01
SET
 weight = (eq_pond_rutas + eq_pond_mall + eq_pond_u)/100; 

------------------------------------
--/* 1.6 Creación tabla output  */--
------------------------------------

CREATE TABLE output.paraderos AS
(SELECT stop_id, stop_name, geom, weight as wj
FROM ahp_paraderos_422
ORDER BY stop_id);

CREATE TABLE output.paraderos_g08 AS
(SELECT stop_id, stop_name, geom, weight as wj
FROM ahp_paraderos_g08
ORDER BY stop_id);

CREATE TABLE output.paraderos_i09 AS
(SELECT stop_id, stop_name, geom, weight as wj
FROM ahp_paraderos_i09
ORDER BY stop_id);

CREATE TABLE output.paraderos_c01 AS
(SELECT stop_id, stop_name, geom, weight as wj
FROM ahp_paraderos_c01
ORDER BY stop_id);

------------------------------
--/* 2.- ZONAS CENSALES */----
------------------------------

----------------------------------------------------------------------------------------
--/* 2.1.- Tabla con centroides de cada zona censal y la suma de viajes bip por día */--
----------------------------------------------------------------------------------------

CREATE TABLE stgo_centroids_bip AS
(SELECT zc.id, ST_Transform(ST_Centroid(zc.geom),4326) as geom,
			zc.nom_comuna,
			SUM(bip.subidas) AS subidas, 
			SUM(bip.bajadas) AS bajadas
FROM zonas_censales_rm AS zc
LEFT JOIN viajes_bip AS bip
ON ST_Intersects(zc.geom, 
				 ST_Transform(bip.geom, 32719))
WHERE (nom_provin = 'SANTIAGO' OR nom_comuna = 'SAN BERNARDO' OR nom_comuna = 'PUENTE ALTO')
GROUP BY zc.id, zc.geom);

-- Columna de viajes totales
ALTER TABLE 
  stgo_centroids_bip 
ADD 
  COLUMN total_bip float;

UPDATE 
  stgo_centroids_bip 
SET 
  total_bip = subidas + bajadas;
  
  
------------------------------------------------------------
--/* 2.2.- Ai de cada centroide de las zonas de demanda */--
------------------------------------------------------------

-- Para cada centroide de demanda se ven qué paraderos quedan a 500 metros de él
-- Luego se ve la distancia a cada uno de los paraderos y se multiplica por su respectivo peso

CREATE TABLE demanda_w_d AS
(SELECT d.id, d.stop_id, ahp.weight, d.dist
FROM
	(SELECT demanda.id, paraderos.stop_id, ROUND(ST_Distance(ST_Transform(paraderos.geom, 32719),
								ST_Transform(demanda.geom, 32719))::numeric,2) AS dist
	FROM ahp_paraderos_422 as paraderos
	LEFT JOIN stgo_centroids_bip as demanda
	ON ST_Intersects(ST_Buffer(ST_Transform(demanda.geom, 32719), 500), 
					 ST_Transform(paraderos.geom, 32719))) AS d
LEFT JOIN ahp_paraderos_422 as ahp
ON ahp.stop_id = d.stop_id);

-- g08

CREATE TABLE demanda_w_d_g08 AS
(SELECT d.id, d.stop_id, ahp.weight, d.dist
FROM
	(SELECT demanda.id, paraderos.stop_id, ROUND(ST_Distance(ST_Transform(paraderos.geom, 32719),
								ST_Transform(demanda.geom, 32719))::numeric,2) AS dist
	FROM ahp_paraderos_g08 as paraderos
	LEFT JOIN stgo_centroids_bip as demanda
	ON ST_Intersects(ST_Buffer(ST_Transform(demanda.geom, 32719), 500), 
					 ST_Transform(paraderos.geom, 32719))) AS d
LEFT JOIN ahp_paraderos_g08 as ahp
ON ahp.stop_id = d.stop_id);


-- i09
CREATE TABLE demanda_w_d_i09 AS
(SELECT d.id, d.stop_id, ahp.weight, d.dist
FROM
	(SELECT demanda.id, paraderos.stop_id, ROUND(ST_Distance(ST_Transform(paraderos.geom, 32719),
								ST_Transform(demanda.geom, 32719))::numeric,2) AS dist
	FROM ahp_paraderos_i09 as paraderos
	LEFT JOIN stgo_centroids_bip as demanda
	ON ST_Intersects(ST_Buffer(ST_Transform(demanda.geom, 32719), 500), 
					 ST_Transform(paraderos.geom, 32719))) AS d
LEFT JOIN ahp_paraderos_i09 as ahp
ON ahp.stop_id = d.stop_id);


-- c01

CREATE TABLE demanda_w_d_c01 AS
(SELECT d.id, d.stop_id, ahp.weight, d.dist
FROM
	(SELECT demanda.id, paraderos.stop_id, ROUND(ST_Distance(ST_Transform(paraderos.geom, 32719),
								ST_Transform(demanda.geom, 32719))::numeric,2) AS dist
	FROM ahp_paraderos_c01 as paraderos
	LEFT JOIN stgo_centroids_bip as demanda
	ON ST_Intersects(ST_Buffer(ST_Transform(demanda.geom, 32719), 500), 
					 ST_Transform(paraderos.geom, 32719))) AS d
LEFT JOIN ahp_paraderos_c01 as ahp
ON ahp.stop_id = d.stop_id);

-- Se añden las nuevas columnas
ALTER TABLE demanda_w_d
ADD COLUMN weight_a FLOAT,
ADD COLUMN dist_b FLOAT,
ADD COLUMN weight_dist FLOAT;

ALTER TABLE demanda_w_d_g08
ADD COLUMN weight_a FLOAT,
ADD COLUMN dist_b FLOAT,
ADD COLUMN weight_dist FLOAT;

ALTER TABLE demanda_w_d_i09
ADD COLUMN weight_a FLOAT,
ADD COLUMN dist_b FLOAT,
ADD COLUMN weight_dist FLOAT;

ALTER TABLE demanda_w_d_c01
ADD COLUMN weight_a FLOAT,
ADD COLUMN dist_b FLOAT,
ADD COLUMN weight_dist FLOAT;

-- Se poblan las columnas
UPDATE demanda_w_d
SET weight_a = weight ^ 2,
	dist_b = dist^(-2);

UPDATE demanda_w_d
SET weight_dist = weight_a * dist_b;

-- g08

UPDATE demanda_w_d_g08
SET weight_a = weight ^ 2,
	dist_b = dist^(-2);

UPDATE demanda_w_d_g08
SET weight_dist = weight_a * dist_b;

-- i09

UPDATE demanda_w_d_i09
SET weight_a = weight ^ 2,
	dist_b = dist^(-2);

UPDATE demanda_w_d_i09
SET weight_dist = weight_a * dist_b;

-- c01

UPDATE demanda_w_d_c01
SET weight_a = weight ^ 2,
	dist_b = dist^(-2);

UPDATE demanda_w_d_c01
SET weight_dist = weight_a * dist_b;

-- Se genera la tabla ai para cada nodo de demanda

CREATE TABLE ai AS
(SELECT bip.*, dwd.ai
FROM
	(SELECT d.id, SUM(weight_dist) AS ai
	FROM demanda_w_d AS d
	GROUP BY d.id) AS dwd
RIGHT JOIN stgo_centroids_bip AS bip
ON dwd.id = bip.id
WHERE bip.nom_comuna = 'PEÑALOLÉN' OR
	  bip.nom_comuna = 'SANTIAGO' OR
	  bip.nom_comuna = 'LO PRADO' OR
	  bip.nom_comuna = 'QUINTA NORMAL' OR
	  bip.nom_comuna = 'PUDAHUEL' OR
 	  bip.nom_comuna = 'ESTACIÓN CENTRAL' OR
 	  bip.nom_comuna = 'PROVIDENCIA' OR
	  bip.nom_comuna ='CERRO NAVIA' OR
	  bip.nom_comuna = 'ÑUÑOA' OR
	  bip.nom_comuna = 'LA REINA');

-- g08

CREATE TABLE ai_g08 AS
(SELECT bip.*, dwd.ai
FROM
	(SELECT d.id, SUM(weight_dist) AS ai
	FROM demanda_w_d_g08 AS d
	GROUP BY d.id) AS dwd
RIGHT JOIN stgo_centroids_bip AS bip
ON dwd.id = bip.id
WHERE bip.nom_comuna = 'SAN BERNARDO' OR
	  bip.nom_comuna = 'LA PINTANA' OR
	  bip.nom_comuna = 'EL BOSQUE' OR
	  bip.nom_comuna = 'SAN RAMÓN' OR
	  bip.nom_comuna = 'LA CISTERNA');

-- i09

CREATE TABLE ai_i09 AS
(SELECT bip.*, dwd.ai
FROM
	(SELECT d.id, SUM(weight_dist) AS ai
	FROM demanda_w_d_i09 AS d
	GROUP BY d.id) AS dwd
RIGHT JOIN stgo_centroids_bip AS bip
ON dwd.id = bip.id
WHERE bip.nom_comuna = 'MAIPÚ' OR
	  bip.nom_comuna = 'CERRILLOS' OR
	  bip.nom_comuna = 'ESTACIÓN CENTRAL' OR
	  bip.nom_comuna = 'LO PRADO' OR
	  bip.nom_comuna = 'PUDAHUEL' OR
	  bip.nom_comuna = 'SANTIAGO');

-- c01

CREATE TABLE ai_c01 AS
(SELECT bip.*, dwd.ai
FROM
	(SELECT d.id, SUM(weight_dist) AS ai
	FROM demanda_w_d_c01 AS d
	GROUP BY d.id) AS dwd
RIGHT JOIN stgo_centroids_bip AS bip
ON dwd.id = bip.id
WHERE bip.nom_comuna = 'LO BARNECHEA' OR
	  bip.nom_comuna = 'LAS CONDES' OR
	  bip.nom_comuna = 'VITACURA' OR
 	  bip.nom_comuna = 'ÑUÑOA' OR
	  bip.nom_comuna = 'PROVIDENCIA');

-- Se cambian los valores null

UPDATE ai
SET ai = (SELECT MIN(ai.ai) FROM ai)
WHERE ai IS NULL;

UPDATE ai
SET total_bip = 0
WHERE total_bip IS NULL;

-- g08

UPDATE ai_g08
SET ai = (SELECT MIN(ai.ai) FROM ai)
WHERE ai IS NULL;

UPDATE ai_g08
SET total_bip = 0
WHERE total_bip IS NULL;

-- I09

UPDATE ai_i09
SET ai = (SELECT MIN(ai.ai) FROM ai)
WHERE ai IS NULL;

UPDATE ai_i09
SET total_bip = 0
WHERE total_bip IS NULL;

-- C01

UPDATE ai_c01
SET ai = (SELECT MIN(ai.ai) FROM ai)
WHERE ai IS NULL;

UPDATE ai_c01
SET total_bip = 0
WHERE total_bip IS NULL;

--------------------------------------------
--/* 2.3.- Tabla final de capa de zonas */--
--------------------------------------------

CREATE TABLE output.nodos_demanda AS
(SELECT id AS zc_id, geom, nom_comuna, total_bip AS ai, ai AS ni
FROM ai
ORDER BY id);

-- g08

CREATE TABLE output.nodos_demanda_g08 AS
(SELECT id AS zc_id, geom, nom_comuna, total_bip AS ai, ai AS ni
FROM ai_g08
ORDER BY id);

--i09

CREATE TABLE output.nodos_demanda_i09 AS
(SELECT id AS zc_id, geom, nom_comuna, total_bip AS ai, ai AS ni
FROM ai_i09
ORDER BY id);

-- c01

CREATE TABLE output.nodos_demanda_c01 AS
(SELECT id AS zc_id, geom, nom_comuna, total_bip AS ai, ai AS ni
FROM ai_c01
ORDER BY id);

-----------------------------------
--/* 3.- MATRIZ DE DISTANCIA */----
-----------------------------------

CREATE TABLE output.dij AS
(SELECT
  demanda.id AS zc_id,
  paraderos.stop_id,
  ST_Distance(ST_Transform(paraderos.geom, 32719), ST_Transform(demanda.geom, 32719)) AS distance
FROM ahp_paraderos_422 AS paraderos
CROSS JOIN ai AS demanda
ORDER BY zc_id, stop_id);

-- g08

CREATE TABLE output.dij_g08 AS
(SELECT
  demanda.id AS zc_id,
  paraderos.stop_id,
  ST_Distance(ST_Transform(paraderos.geom, 32719), ST_Transform(demanda.geom, 32719)) AS distance
FROM ahp_paraderos_g08 AS paraderos
CROSS JOIN ai_g08 AS demanda
ORDER BY zc_id, stop_id);

-- i09

CREATE TABLE output.dij_i09 AS
(SELECT
  demanda.id AS zc_id,
  paraderos.stop_id,
  ST_Distance(ST_Transform(paraderos.geom, 32719), ST_Transform(demanda.geom, 32719)) AS distance
FROM ahp_paraderos_i09 AS paraderos
CROSS JOIN ai_i09 AS demanda
ORDER BY zc_id, stop_id);

-- c01

CREATE TABLE output.dij_c01 AS
(SELECT
  demanda.id AS zc_id,
  paraderos.stop_id,
  ST_Distance(ST_Transform(paraderos.geom, 32719), ST_Transform(demanda.geom, 32719)) AS distance
FROM ahp_paraderos_c01 AS paraderos
CROSS JOIN ai_c01 AS demanda
ORDER BY zc_id, stop_id);

