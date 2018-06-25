satisfaction = LOAD '/seoul/satisfaction.csv' USING PigStorage(',');


seoul = FILTER satisfaction BY $1 MATCHES 'seoul';


sort = FOREACH seoul GENERATE
$0 AS year:double,
$1 AS city:chararray,
$3 AS total:double,
$4 AS transfer:double,
$5 AS stop:double,
$6 AS runtime:double,
$7 AS cometime:double,
$8 AS cost:double;



STORE sort INTO '/seoul/result_satis' USING PigStorage(',');
