congested = LOAD '/seoul/congested.csv' USING PigStorage(',');


seoul = FILTER congested BY $1 MATCHES 'seoul';


sort = FOREACH seoul GENERATE
$0 AS year:double,
$1 AS city:chararray,
$2 AS average:double,
$3 AS metroaverage:double,
$4 AS line1:double,
$5 AS line2:double,
$6 AS line3:double,
$7 AS line4:double;




STORE sort INTO '/seoul/result' USING PigStorage(',');
