#! /bin/bash

test_datetime=$(date +%Y%m%d%H%M%S)
dest_filename="${test_datetime}_hft.csv"
scp root@172.31.32.247:/root/timestamp/result.txt $dest_filename

exit 0

