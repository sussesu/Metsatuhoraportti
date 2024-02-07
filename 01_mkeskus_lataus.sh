module load geoconda

### -- lataa uusin mk-ilmodata

http_mk=https://avoin.metsakeskus.fi/aineistot/Metsankayttoilmoitukset/Maakunta/
kohde_dir=/scratch/project_2008416/Metsatuhoraportti2024/data/MK/
sdamage_dir=/scratch/project_2008416/Metsatuhoraportti2024/data/stormdamage/
mfile=$sdamage_dir"full_finland_2024.gpkg"

cd $kohde_dir

wget -r -l1 --no-parent -A "*.zip" -nd -nc $http_mk

unzip "*.zip"

## -- get rid of special characters in file names
rename - _ *.gpkg
rename ä a *.gpkg
rename ö o *.gpkg
rename Д a *.gpkg
rename +? a *.gpkg
rename +? a *.gpkg
rename +? a *.gpkg

## -- valikoi myrskytapaukset ja yhdistä koko Suomi
#
#fls=$(ls *.gpkg)
#sql_clause="
##'("forestdamagequalifier"=1504) OR ("cuttingrealizationpractice" IN (20, 21))'

##test
#ogr2ogr -sql "SELECT * FROM forestusedeclaration WHERE (forestdamagequalifier == 1504 ) AND (cuttingrealizationpractice IN (20, 21))" -f "GPKG" $sdamage_dir$fl $fl -overwrite

#ogr2ogr -where "forestdamagequalifier"=1504 -f "GPKG" $sdamage_dir$fl $fl -overwrite

#for fl in $fls
#do
#  ogr2ogr -where $where_clause -f "GPKG" $sdamage_dir$fl $fl -overwrite
#  echo $fl
#  if test ! -e $mfile #[$mfile -e]
#  then
#    echo "creating merge file"
#    ogr2ogr -f "GPKG" $mfile $sdamage_dir$fl
#  else
#    echo "merging..."
#    ogr2ogr -f "GPKG" -update -append $mfile $sdamage_dir$fl
#  fi
#done

