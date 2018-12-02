library(swat)
library(ggplot2)
library(reshape2)

library(stats4)
library(readr)

Sys.setenv(CAS_CLIENT_SSL_CA_LIST="C:/dev/viyacertificates/vault-ca.crt")

mydf <- read_csv("data/train.csv")
mydf_names <- names(mydf)

nrow(mydf)
# number of hh with no Target:
nrow(mydf[is.na(mydf$Target),])

##########################################################
# PERSONS
##########################################################

person_df = data.frame(mydf$idhogar,                 # household_id
                       mydf$Target,
                       mydf$dis,                     # disable
                       mydf$age,
                       ifelse(mydf$male==1,'M','F'), # sex
                       ifelse(mydf$estadocivil1==1,1,ifelse(mydf$estadocivil2==1,2,ifelse(mydf$estadocivil3==1,3,ifelse(mydf$estadocivil4==1,4,ifelse(mydf$estadocivil5==1,5,ifelse(mydf$estadocivil6==1,6,ifelse(mydf$estadocivil7==1,7,0))))))),
                       ifelse(mydf$parentesco1==1,1,ifelse(mydf$parentesco2==1,2,ifelse(mydf$parentesco3==1,3,ifelse(mydf$parentesco4==1,4,ifelse(mydf$parentesco5==1,5,ifelse(mydf$parentesco6==1,6,ifelse(mydf$parentesco7==1,7,ifelse(mydf$parentesco8==1,8,ifelse(mydf$parentesco9==1,9,ifelse(mydf$parentesco10==1,10,ifelse(mydf$parentesco11==1,11,ifelse(mydf$parentesco12==1,12,0)))))))))))),
                       ifelse(mydf$instlevel1==1,1,ifelse(mydf$instlevel2==1,2,ifelse(mydf$instlevel3==1,3,ifelse(mydf$instlevel4==1,4,ifelse(mydf$instlevel5==1,5,ifelse(mydf$instlevel6==1,6,ifelse(mydf$instlevel7==1,7,0))))))),
                       mydf$escolari,
                       mydf$rez_esc,
                       mydf$computer,
                       mydf$television,
                       mydf$mobilephone,  # hh_has_mobile
                       mydf$qmobilephone, # hh_num_mobiles
                       mydf$v18q,  # hh_has_tablet
                       mydf$v18q1, # hh_num_tablets
                       mydf$r4h1,  # hh_num_males_under_12
                       mydf$r4h2,  # hh_num_males_over_12
                       mydf$r4h3,  # hh_num_males
                       mydf$r4m1,  # hh_num_females_under_12
                       mydf$r4m2,  # hh_num_females_over_12
                       mydf$r4m3,  # hh_num_females
                       mydf$r4t1,  # hh_num_under_12
                       mydf$r4t2,  # hh_num_over_12
                       mydf$r4t3,  # hh_num_persons
                       mydf$tamhog, # hh_size
                       mydf$tamviv, # hh_num_living_persons
                       mydf$hogar_nin,    # hh_num_children_under_19 = num of children 0 to 19 in household
                       mydf$hogar_adul,   # hh_num_adults = num of adults in household
                       mydf$hogar_mayor,  # hh_num_senior_over_65 = num of individuals 65+ in the household
                       mydf$hogar_total,  # hh_num_individuals_household = total individuals in the household
                       mydf$dependency,   # hh_dependency_rate = Dependency rate
                       ifelse(mydf$edjefe == 'no',mydf$edjefa,mydf$edjefe), # hh_years_education = years of education of household
                       mydf$meaneduc,     # hh_avg_years_education = average years of education for adults (18+)
                       mydf$v2a1,    # hh_house_rent
                       mydf$hacdor,
                       mydf$rooms,
                       mydf$hacapo,
                       mydf$v14a,
                       mydf$refrig,
                       ifelse(mydf$paredblolad==1,1,ifelse(mydf$paredzocalo==1,2,ifelse(mydf$paredpreb==1,3,ifelse(mydf$pareddes==1,4,ifelse(mydf$paredmad==1,5,ifelse(mydf$paredzinc==1,6,ifelse(mydf$paredfibras==1,7,ifelse(mydf$paredother==1,8,0)))))))),
                       ifelse(mydf$pisomoscer==1,1,ifelse(mydf$pisocemento==1,2,ifelse(mydf$pisoother==1,3,ifelse(mydf$pisonatur==1,4,ifelse(mydf$pisonotiene==1,5,ifelse(mydf$pisomadera==1,6,0)))))),
                       ifelse(mydf$techoentrepiso==1,1,ifelse(mydf$techocane==1,2,ifelse(mydf$techootro==1,3,0))),
                       mydf$cielorazo,
                       ifelse(mydf$abastaguadentro==1,1,ifelse(mydf$abastaguafuera==1,2,ifelse(mydf$abastaguano==1,3,0))),
                       ifelse(mydf$public==1,1,ifelse(mydf$planpri==1,2,ifelse(mydf$noelec==1,3,ifelse(mydf$coopele==1,4,0)))),
                       ifelse(mydf$sanitario1==1,1,ifelse(mydf$sanitario2==1,2,ifelse(mydf$sanitario3==1,3,ifelse(mydf$sanitario5==1,5,ifelse(mydf$sanitario6==1,6,0))))),
                       ifelse(mydf$energcocinar1==1,1,ifelse(mydf$energcocinar2==1,2,ifelse(mydf$energcocinar3==1,3,ifelse(mydf$energcocinar4==1,4,0)))),
                       ifelse(mydf$elimbasu1==1,1,ifelse(mydf$elimbasu2==1,2,ifelse(mydf$elimbasu3==1,3,ifelse(mydf$elimbasu4==1,4,ifelse(mydf$elimbasu5==1,5,ifelse(mydf$elimbasu6==1,6,0)))))),
                       ifelse(mydf$epared1==1,1,ifelse(mydf$epared2==1,2,ifelse(mydf$epared3==1,3,0))),
                       ifelse(mydf$etecho1==1,1,ifelse(mydf$etecho2==1,2,ifelse(mydf$etecho3==1,3,0))),
                       ifelse(mydf$eviv1==1,1,ifelse(mydf$eviv2==1,2,ifelse(mydf$eviv3==1,3,0))),
                       mydf$bedrooms,
                       mydf$overcrowding,
                       ifelse(mydf$tipovivi1==1,1,ifelse(mydf$tipovivi2==1,2,ifelse(mydf$tipovivi3==1,3,ifelse(mydf$tipovivi4==1,4,ifelse(mydf$tipovivi5==1,5,0))))),
                       ifelse(mydf$lugar1==1,'Central',ifelse(mydf$lugar2==1,'Chorotega',ifelse(mydf$lugar3==1,'Parafico Central',ifelse(mydf$lugar4==1,'Brunca',ifelse(mydf$lugar5==1,'Huetar Atlantica',ifelse(mydf$lugar6==1,'Huerta Norte','')))))),
                       ifelse(mydf$area1==1,'urban','rural')
                      )

colnames(person_df) <- c('household_id',
                         'Target',
                         'disable',
                         'age',
                         'sex',
                         'civil_status',
                         'relation_type',
                         'instruction_level',
                         'schooling_years',
                         'years_behind_in_school',
                         'hh_has_computer',
                         'hh_has_tv',
                         'hh_has_mobile',
                         'hh_num_mobiles',
                         'hh_has_tablet',
                         'hh_num_tablets',
                         'hh_num_males_under_12',
                         'hh_num_males_over_12',
                         'hh_num_males',
                         'hh_num_females_under_12',
                         'hh_num_females_over_12',
                         'hh_num_females',
                         'hh_num_under_12',
                         'hh_num_over_12',
                         'hh_num_persons',
                         'hh_size',
                         'hh_num_living_persons',
                         'hh_num_children_under_19',
                         'hh_num_adults',
                         'hh_num_senior_over_65',
                         'hh_num_individuals_household',
                         'hh_dependency_rate',
                         'hh_years_education',
                         'hh_avg_years_education',
                         'hh_house_rent',
                         'overcrowding_bedrooms',
                         'num_rooms',
                         'overcrowding_rooms',
                         'has_toilet',
                         'has_refrigerator',
                         'outside_walls',
                         'floor_material',
                         'roof_material',
                         'ceiling',
                         'water_provisioning',
                         'electricity',
                         'toilet',
                         'cooking_energy',
                         'rubbish_disposal',
                         'wall_status',
                         'roof_status',
                         'floor_status',
                         'num_bedrooms',
                         'persons_per_room',
                         'house_property',
                         'region',
                         'area'
                        )


nrow(person_df[person_df$relation_type ==1,])

# number of hh with no Target:
nrow(person_df[is.na(person_df$Target),])

##########################################################
# HOUSEHOLD
##########################################################

hh_head_df <- person_df[person_df$relation_type == 1,]

colnames(hh_head_df) <- colnames(person_df)

nrow(hh_head_df)


# FAMILY INFO

hh_family_df <- aggregate(cbind(parentesco1,parentesco2,parentesco3,parentesco4,parentesco5,parentesco6,parentesco7,parentesco8,parentesco9,parentesco10,parentesco11,parentesco12,estadocivil1,estadocivil2,estadocivil3,estadocivil4,estadocivil5,estadocivil6,estadocivil7,dis,mydf$instlevel1, mydf$instlevel2, mydf$instlevel3, mydf$instlevel4, mydf$instlevel5, mydf$instlevel6, mydf$instlevel7, mydf$instlevel8, mydf$instlevel9 ) ~ idhogar, mydf, sum)

colnames(hh_family_df) <- c('household_id',
                     'num_household_head',
                     'num_spouse_partner',
                     'num_son_doughter',
                     'num_stepson_doughter',
                     'num_son_doughter_in_law',
                     'num_grandson_doughter',
                     'num_mother_father',
                     'num_father_mother_in_law',
                     'num_brother_sister',
                     'num_brother_sister_in_law',
                     'num_other_family_member',
                     'num_other_non_family_member',
                     'num_minor',
                     'num_free_cor_coupled',
                     'num_married',
                     'num_divorced',
                     'num_separated',
                     'num_widow',
                     'num_single',
                     'num_disables',
                     'num_no_education',
                     'num_imcomplete_primary',
                     'num_complete_primary',
                     'num_incomplete_secondary_academic',
                     'num_complete_secondary_academic',
                     'num_incomplete_technical_technical',
                     'num_complete_technical_technical',
                     'num_undergraduate_higher',
                     'num_postgraduate_higher'
                    )

# Full outer join: 
# hh_df <- merge(hh_head_df, hh_family_df, by='household_id', all=TRUE)
hh_df <- merge(hh_head_df, hh_family_df, by='household_id')

# number of hh with no Target:
nrow(hh_df[is.na(hh_df$Target),])
nrow(hh_df)


#################################################################################
# LOAD INTO CAS
#################################################################################

Sys.setenv(CAS_CLIENT_SSL_CA_LIST="C:/dev/viyacertificates/vault-ca.crt")

conn <- CAS('viya34smp.nordiclab.sashq-r.openstack.sas.com', 5570)

cas_households <- as.casTable(conn, hh_df, casOut = list(name="households", promote=TRUE))

#dropTable(cas_households)

hh_df[hh_df$hh_num_living_persons != hh_df$hh_num_individuals_household, c('hh_num_living_persons','hh_num_individuals_household')]

#---------------------------------------------
# Split the Data Into Training and Validation
#---------------------------------------------
# Load the sampling actionset
loadActionSet(conn, 'sampling')

cas.sampling.srs(conn, 
                 table="households",
                 sampPct=70, 
                 partind=TRUE, 
                 output=list(casOut=list(name='households_part', replace=TRUE), copyvars='ALL'))

cas.table.promote(conn,name="households_part")

# Verify the partitioning
# Load the fedsql actionset
loadActionSet(conn, 'fedsql')

# Make sure the partition worked correctly using SQL
cas.fedsql.execDirect(conn, query = paste0(
  "SELECT CASE WHEN _PartInd_ = 1 THEN 'Training' ELSE 'Validation' END AS name, ",
  "_PartInd_, COUNT(*) AS obs FROM hr_part ",
  "GROUP BY CASE WHEN _PartInd_ = 1 THEN 'Training' ELSE 'Validation' END, _PartInd_;")
)$`Result Set`

# Create two CASTable instances
# The defCasTable function is used twice to create two CASTable instances. 
# The instances are not duplicate copies of the Hr_Part in-memory table. 
# They are objects in R and when they are used, the filter that is specified in the where argument 
# is applied to the single instance of the Hr_Part table.
train <- defCasTable(conn, 'households_part', where="_PartInd_=1")
valid <- defCasTable(conn, 'households_part', where="_PartInd_=0")



# Confirm that filtered rows show the same counts
cat("\n")
cat("Rows in training table:", nrow(train), "\n")
cat("Rows in validation table:", nrow(valid), "\n")


