#!/bin/bash

FILE=cms--$(date +%Y-%m-%d).sql.gpg
s3cmd get s3://jekor.com-archive/$FILE
echo "DROP DATABASE cms; CREATE DATABASE cms; GRANT ALL ON DATABASE cms TO cms;" | psql -U postgres -h localhost postgres
gpg2 -d $FILE | psql -U postgres -h localhost cms
rm $FILE