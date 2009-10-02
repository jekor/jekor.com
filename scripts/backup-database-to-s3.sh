#!/bin/bash

FILE=cms--$(date +%Y-%m-%d).sql.gpg
pg_dump -U postgres -h localhost --create cms | gpg2 -e -r "chris@forno.us" > $FILE
s3cmd put $FILE s3://jekor.com-archive/
rm $FILE