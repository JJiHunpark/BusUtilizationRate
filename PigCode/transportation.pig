transportation = LOAD '/seoul/transportation.csv' USING PigStorage(',');


seoul = FILTER transportation BY $1 MATCHES 'seoul';


sort = FOREACH seoul GENERATE
$0 AS year:double,
$1 AS city:chararray,
$7 AS bus:double,
$8 AS metro:double,
$9 AS bm:double;



STORE sort INTO '/seoul/result' USING PigStorage(',');
