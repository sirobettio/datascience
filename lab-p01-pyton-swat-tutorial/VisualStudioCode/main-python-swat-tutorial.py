# -*- coding: utf-8 -*-
"""
Created on Sun Dec  2 16:13:06 2018

@author: sdksbe
"""
import swat
import os

os.environ['CAS_CLIENT_SSL_CA_LIST']=r"C:/dev/viyacertificates/lab01-viya34smp/vault-ca.crt"
#os.environ['REQUESTS_CA_BUNDLE']=r"C:/dev/viyacertificates/lab01-viya34smp/vault-ca.crt"

conn = swat.CAS('viya34smp.nordiclab.sashq-r.openstack.sas.com', 5570)
csvtbl = conn.read_csv('https://raw.githubusercontent.com/sassoftware/sas-viya-programming/master/data/cars.csv')
h = csvtbl.head()
print(h)
r=conn.session.endSession()
print(r)