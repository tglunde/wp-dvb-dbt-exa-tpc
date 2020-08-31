{{ config(materialized='view') }}

SELECT
	cntrycode,
	count(*) AS numcust,
	sum(c_acctbal) AS totacctbal
FROM
	(
	SELECT
		SUBSTRING(c_phone FROM 1 FOR 2) AS cntrycode, c_acctbal
	FROM
		BUSINESSOBJECTS.CUSTOMER_S_EXA_STAGE
	WHERE
		SUBSTRING(c_phone FROM 1 FOR 2) IN ('13',
		'31',
		'23',
		'29',
		'30',
		'18',
		'17')
		AND c_acctbal > (
		SELECT
			avg(c_acctbal)
		FROM
			BUSINESSOBJECTS.CUSTOMER_S_EXA_STAGE
		WHERE
			c_acctbal > 0.00
			AND SUBSTRING (c_phone
		FROM
			1 FOR 2) IN ('13',
			'31',
			'23',
			'29',
			'30',
			'18',
			'17') )
		AND NOT EXISTS (
		SELECT
			*
		FROM
			BUSINESSOBJECTS.ORDER_S_EXA_STAGE
		WHERE
			o_custkey = c_custkey ) ) AS custsale
GROUP BY
	cntrycode
ORDER BY
	cntrycode