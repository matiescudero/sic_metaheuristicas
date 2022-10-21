/* Se crean las tablas del GTFS */

DROP TABLE IF EXISTS gtfs.agency;

CREATE TABLE gtfs.agency (
	agency_id VARCHAR(255),
	agency_name VARCHAR(255),
	agency_url VARCHAR(255),
	agency_timezone VARCHAR(255)
);

DROP TABLE IF EXISTS gtfs.calendar_dates;

CREATE TABLE gtfs.calendar_dates (
	service_id VARCHAR(255),
	"date" VARCHAR(255),
	exception_type VARCHAR(255)
);

DROP TABLE IF EXISTS gtfs.feed_info;

CREATE TABLE gtfs.feed_info (
    feed_publisher_name VARCHAR(255),
    feed_publisher_url VARCHAR(255),
    feed_lang VARCHAR(255),
    feed_start_date VARCHAR(255),
    feed_end_date VARCHAR(255),
    feed_version VARCHAR(255)
);

DROP TABLE IF EXISTS gtfs.routes;

CREATE TABLE gtfs.routes (
    route_id VARCHAR(255) PRIMARY KEY,
    agency_id VARCHAR(255),
    route_short_name VARCHAR(255),
    route_long_name VARCHAR(255),
	route_desc VARCHAR(255),
    route_type VARCHAR(255),
	route_url VARCHAR(255),
    route_color VARCHAR(255),
    route_text_color VARCHAR(255)
);

DROP TABLE IF EXISTS gtfs.shapes;

CREATE TABLE gtfs.shapes (
    shape_id VARCHAR(255),
    shape_pt_lat VARCHAR(255),
    shape_pt_lon VARCHAR(255),
    shape_pt_sequence VARCHAR(255)
);

DROP TABLE IF EXISTS gtfs.stop_times;

CREATE TABLE gtfs.stop_times (
    trip_id VARCHAR(255),
    arrival_time VARCHAR(255),
    departure_time VARCHAR(255),
    stop_id VARCHAR(255),
    stop_sequence VARCHAR(255)
);

DROP TABLE IF EXISTS gtfs.stops;

CREATE TABLE gtfs.stops (
    stop_id VARCHAR(255),
	stop_code VARCHAR(255),
    stop_name VARCHAR(255),
    stop_lat VARCHAR(255),
    stop_lon VARCHAR(255),
    stop_url VARCHAR(255),
    wheelchair_boarding VARCHAR(255)
);


DROP TABLE IF EXISTS gtfs.trips;

CREATE TABLE gtfs.trips (
    route_id VARCHAR(255),
    service_id VARCHAR(255),
    trip_id VARCHAR(255),
    trip_headsign VARCHAR(255),
    direction_id VARCHAR(255),
    shape_id VARCHAR(255)
);

/* Carga Capas */

COPY gtfs.agency
FROM 'C:/Users/Public/gtfs/agency.txt' DELIMITER ',' CSV HEADER;

COPY gtfs.calendar_dates
FROM 'C:/Users/Public/gtfs/calendar_dates.txt' DELIMITER ',' CSV HEADER;

COPY gtfs.feed_info
FROM 'C:/Users/Public/gtfs/feed_info.txt' DELIMITER ',' CSV HEADER;

COPY gtfs.routes
FROM 'C:/Users/Public/gtfs/routes.txt' DELIMITER ',' CSV HEADER;

COPY gtfs.shapes
FROM 'C:/Users/Public/gtfs/shapes.txt' DELIMITER ',' CSV HEADER;

COPY gtfs.stop_times
FROM 'C:/Users/Public/gtfs/stop_times.txt' DELIMITER ',' CSV HEADER;

COPY gtfs.stops
FROM 'C:/Users/Public/gtfs/stops.txt' DELIMITER ',' CSV HEADER;

COPY gtfs.trips
FROM 'C:/Users/Public/gtfs/trips.txt' DELIMITER ',' CSV HEADER;

/* Se añade campo de geometría */

-- Se añade columna de geometría
ALTER TABLE gtfs.stops ADD COLUMN geom geometry(Point, 4326);

-- Se pobla el campo geom
UPDATE gtfs.stops SET geom = ST_MakePoint(stop_lon::float, stop_lat::float);

