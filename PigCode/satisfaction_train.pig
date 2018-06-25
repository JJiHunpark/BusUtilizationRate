satisfaction_train = LOAD '/seoul/satisfaction_train.csv' USING PigStorage(',');


seoul = FILTER congested BY $1 MATCHES 'seoul';


sort = FOREACH seoul GENERATE
$0 AS year:double,
$1 AS city:chararray,
$3 AS public:double,
$4 AS bus_satis:double,
$5 AS metro_satis:double,
$6 AS taxi_satis:double;




STORE sort INTO '/seoul/result' USING PigStorage(',');
