#!/usr/bin/env bash
#
# Initialize data download. Run from home directory after clonining repository
#
#   ./load.sh
#

set -e
set -x

INSTALL_HOME=$(pwd)

mkdir -p ./data/raw

cd ./data/raw

# NSUDH 2017 Data Download
curl -OL http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads-protected/studies/NSDUH-2017/NSDUH-2017-datasets/NSDUH-2017-DS0001/NSDUH-2017-DS0001-info/NSDUH-2017-DS0001-info-codebook.pdf
curl -OL http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads-protected/studies/NSDUH-2017/NSDUH-2017-datasets/NSDUH-2017-DS0001/NSDUH-2017-DS0001-bundles-with-study-info/NSDUH-2017-DS0001-bndl-data-tsv.zip
unzip NSDUH-2017-DS0001-bndl-data-tsv.zip

