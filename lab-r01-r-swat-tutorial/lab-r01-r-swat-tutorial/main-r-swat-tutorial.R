library(swat)

Sys.setenv(CAS_CLIENT_SSL_CA_LIST="C:/dev/viyacertificates/lab01-viya34smp/vault-ca.crt")

# Connect to CAS 
conn <- swat::CAS('viya34smp.nordiclab.sashq-r.openstack.sas.com', 8777, username="siro", password="Orion123", protocol="https") 

# Load Iris data set into in-memory table 
iris.ct <- as.casTable(conn, iris) 

# Use basic R functions on CAS table 
min(iris.ct$Sepal.Length) 

max(iris.ct$Sepal.Length)

mean(iris.ct$Sepal.Length)

# Call CAS actions on the table 
out <- cas.simple.summary(iris.ct) 
out

out$Summary[c('Column', 'Min', 'Max')]

# Simple summary action
cas.simple.summary(iris.ct, subset=c("min", "max", "mean"))
cas.percentile.percentile(iris.ct, values=c(25, 50, 75))


# An important feature of CAS is the ability to process grouped data.
# The following statements show how to perform the following:
# - Create another instance of a CASTable object that references the in-memory data in CAS. 
#   The instance is defined to perform BY-group processing. 
#   The key concept is that the second instance does not duplicate the in-memory data; it specifies that 
#   BY-group processing on the species variable is performed with the existing in-memory table.
# - Produce summary statistics for the in-memory table.
#   Because BY-group processing is performed, the results include a casDataFrame for each species of Iris 
#   and one casDataFrame that is named ByGroupInfo.
# - The ByGroupInfo that relates each group to the casDataFrame that shows the results for the group. 
#   In the following example, results for Setosa are included in the result that is named ByGroup1.Summary.
iris.ct.grouped <- defCasTable(conn, table="iris", groupby=c("species"))
groups <- cas.simple.summary(iris.ct.grouped, subset=c("min", "max", "mean"))
groups$ByGroupInfo
groups$ByGroup1.Summary

res <- cas.builtins.serverStatus(conn)
length(res)
name(res)
res$server
res$nodestatus

#---------------------------------------------
# End the session
#---------------------------------------------
cas.session.endSession(conn)
