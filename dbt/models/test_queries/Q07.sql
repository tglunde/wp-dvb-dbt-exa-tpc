{{ config(materialized='view') }}

SELECT
	supp_nation,
	cust_nation,
	l_year,
	sum(volume) AS revenue
FROM
	(
	SELECT
		n1.n_name AS supp_nation, n2.n_name AS cust_nation, EXTRACT(YEAR
	FROM
		l_shipdate) AS l_year, l_extendedprice * (1 - l_discount) AS volume
	FROM
		--BUSINESSOBJECTS.SUPPLIER_S_EXA_STAGE, BUSINESSOBJECTS.LINEITEM_S_EXA_STAGE, BUSINESSOBJECTS.ORDER_S_EXA_STAGE, BUSINESSOBJECTS.CUSTOMER_S_EXA_STAGE, BUSINESSOBJECTS.NATION_S_EXA_STAGE n1, BUSINESSOBJECTS.NATION_S_EXA_STAGE n2
		{{ source('accesslayer','supplier')}}, {{ source('accesslayer','lineitem')}}, {{ source('accesslayer','order')}}, {{ source('accesslayer','customer')}}, {{ source('accesslayer','nation')}} n1, {{ source('accesslayer','nation')}} n2
	WHERE
		s_suppkey = l_suppkey
		AND o_orderkey = l_orderkey
		AND c_custkey = o_custkey
		AND s_nationkey = n1.n_nationkey
		AND c_nationkey = n2.n_nationkey
		AND ( (n1.n_name = 'FRANCE'
		AND n2.n_name = 'GERMANY')
		OR (n1.n_name = 'GERMANY'
		AND n2.n_name = 'FRANCE') )
		AND l_shipdate BETWEEN DATE '1995-01-01' AND DATE '1996-12-31' ) AS shipping
GROUP BY
	supp_nation,
	cust_nation,
	l_year
ORDER BY
	supp_nation,
	cust_nation,
	l_year