module load geoconda

### -- lataa uusin mk-ilmodata

http_mk=https://avoin.metsakeskus.fi/aineistot/Metsankayttoilmoitukset/Maakunta/
kohde_dir=/scratch/project_2008416/Metsatuhoraportti/data/MK2025
sdamage_dir=/scratch/project_2008416/Metsatuhoraportti/data/MK2025/tuhot/
mfile_storm=$sdamage_dir"storms_full_finland_2025.gpkg"

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
rename ├д a *.gpkg

# ## -- valikoi myrskytapaukset ja yhdistä koko Suomi    !!!! THIS IS NOW DONE IN THE R SCRIPT (02_process_data.R) !!!!
# 
# fls=$(ls *.gpkg)
# sql_clause='("forestdamagequalifier"=1504) OR ("cuttingrealizationpractice" IN (20, 21))'
# 
# # ##test
# # ogr2ogr -sql "SELECT * FROM forestusedeclaration WHERE (forestdamagequalifier == 1504 ) AND (cuttingrealizationpractice IN (20, 21))" -f "GPKG" $sdamage_dir$fl $fl -overwrite
# # ogr2ogr -where "forestdamagequalifier"=1504 -f "GPKG" $sdamage_dir$fl $fl -overwrite
# 
# for fl in $fls
# do
#  ogr2ogr -where $sql_clause -f "GPKG" $sdamage_dir$fl $fl -overwrite
#  echo $fl
#  if test ! -e $mfile_storm #[$mfile_storm -e]
#  then
#    echo "creating merge file"
#    ogr2ogr -f "GPKG" $mfile_storm $sdamage_dir$fl
#  else
#    echo "merging..."
#    ogr2ogr -f "GPKG" -update -append $mfile_storm $sdamage_dir$fl
#  fi
# done

