-- DROP FUNCTION wrk_fromAtoB(text, text, integer, integer);

CREATE OR REPLACE FUNCTION wrk_fromAtoB(
    IN start_pt TEXT,
    IN end_pt TEXT,
    IN in_sid INTEGER,
    IN search_buffer INTEGER,
    OUT sid INTEGER,

    OUT geom geometry
)
RETURNS SETOF record AS
$BODY$
DECLARE
    final_query TEXT;
BEGIN

    final_query :=
        FORMAT( $$
            WITH
            dijkstra AS (
                SELECT *
                FROM pgr_dijkstra(
                    'SELECT
                      id,
                      source,
                      target,
                      1 as cost
                    FROM vis_graph
                    WHERE the_geom && ST_Expand(
                      (ST_Collect(ST_GeomFromEWKT(''%1$s''),ST_GeomFromEWKT(''%2$s'')) ) ,
                      ''%4$s'')'::text,
                    -- source
                    (SELECT id FROM vis_graph_vertices_pgr
                        ORDER BY the_geom <-> ST_GeomFromEWKT('%1$s') LIMIT 1),
                    -- target
                    (SELECT id FROM vis_graph_vertices_pgr
                        ORDER BY the_geom <-> ST_GeomFromEWKT('%2$s') LIMIT 1),
                        false)
            ),
            get_geom AS (
                SELECT dijkstra.*,
                  CASE
                    WHEN dijkstra.node = vis_graph.source THEN the_geom
                    ELSE ST_Reverse(the_geom)
                END AS route_geom
                FROM dijkstra JOIN vis_graph ON (edge = vis_graph.id)
                ORDER BY seq)
            SELECT
                %3$s sid,
                CASE
                  WHEN (SELECT id FROM vis_graph_vertices_pgr
                        ORDER BY the_geom <-> ST_GeomFromEWKT('%1$s') LIMIT 1) =
                       (SELECT id FROM vis_graph_vertices_pgr
                        ORDER BY the_geom <-> ST_GeomFromEWKT('%2$s') LIMIT 1) THEN
                    st_makeline(
                    ARRAY[ST_GeomFromEWKT('%1$s'),
                    (SELECT the_geom FROM vis_graph_vertices_pgr
                        ORDER BY the_geom <-> ST_GeomFromEWKT('%1$s') LIMIT 1),
                    ST_GeomFromEWKT('%2$s')])
                  ELSE
                    st_addpoint(
                    st_addpoint(
                    st_linemerge(st_collect(route_geom)),ST_GeomFromEWKT('%1$s'),0),
                    ST_GeomFromEWKT('%2$s'))
                END AS the_geom
            FROM get_geom JOIN vis_graph ON get_geom.edge = vis_graph.id;$$,
        start_pt, end_pt, in_sid, search_buffer); -- %1 to %3 of the FORMAT function
    -- RAISE notice '%', final_query;
    RETURN QUERY EXECUTE final_query;
END;
$BODY$
LANGUAGE 'plpgsql';
