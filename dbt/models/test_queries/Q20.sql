{{ config(materialized='view') }}

SELECT
	s_name,
	s_address
FROM
	BUSINESSOBJECTS.SUPPLIER_S_EXA_STAGE,
	BUSINESSOBJECTS.NATION_S_EXA_STAGE
WHERE
	s_suppkey IN (
	SELECT
		ps_suppkey
	FROM
		BUSINESSOBJECTS.PARTSUPP_S_EXA_STAGE
	WHERE
		ps_partkey IN (
		SELECT
			p_partkey
		FROM
			BUSINESSOBJECTS.PART_S_EXA_STAGE
		WHERE
			p_name LIKE 'forest%' )
		AND ps_availqty > (
		SELECT
			0.5 * sum(l_quantity)
		FROM
			BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE
		WHERE
			l_partkey = ps_partkey
			AND l_suppkey = ps_suppkey
			AND l_shipdate >= DATE '1994-01-01'
			AND l_shipdate < DATE '1994-01-01' + INTERVAL '1' YEAR ) )
	AND s_nationkey = n_nationkey
	AND n_name = 'CANADA'
ORDER BY
	s_name