{{ config(materialized='view') }}

SELECT
	nation,
	o_year,
	sum(amount) AS sum_profit
FROM
	(
	SELECT
		n_name AS nation, EXTRACT(YEAR
	FROM
		o_orderdate) AS o_year, l_extendedprice * (1 - l_discount) - ps_supplycost * l_quantity AS amount
	FROM
		BUSINESSOBJECTS.PART_S_EXA_STAGE, BUSINESSOBJECTS.SUPPLIER_S_EXA_STAGE, BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE, BUSINESSOBJECTS.PARTSUPP_S_EXA_STAGE, BUSINESSOBJECTS.ORDER_S_EXA_STAGE, BUSINESSOBJECTS.NATION_S_EXA_STAGE
	WHERE
		s_suppkey = l_suppkey
		AND ps_suppkey = l_suppkey
		AND ps_partkey = l_partkey
		AND p_partkey = l_partkey
		AND o_orderkey = l_orderkey
		AND s_nationkey = n_nationkey
		AND p_name LIKE '%green%' ) AS profit
GROUP BY
	nation,
	o_year
ORDER BY
	nation,
	o_year DESC