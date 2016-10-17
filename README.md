Currently this package is broken because sample data has been removed. It is awaiting maintenance. 

# Package svdc

SVAMP data cleaning (svdc) is a package that cleans data from the SVA
oracle database in a way that makes it the correct input data to the
subsequent mapping functionality.

***

### Data sources for data_cleaning function
Details about input data of  data_cleaning function are listed below. For each function's arguments source, path to dataset and download details are provided.

#### 1. PPN dataset

* Argument name: ppn_dataset
* Path: "//UBUNTU1/share/PPN_records.csv"
* Source: Rapportportalen

Details
Rubrik: PPN - Utdrag med intressent  
Dokumenttyp: Text  
Status Återkommande  
Destination: Filkopiera instansen med filnamnet: "PPN_records.csv" till katalogen: "//ubuntu1/share".  
Ägare: thomas.rosendal  
Skapad: 2014-11-25 09:10  
Nästa körningstid: 2015-12-08 05:00  
Typ av upprepning: Objektet körs en gång var 1:e dag.  
Sökväg till överordnat objekt: SVA/PPN/  
Fjärrinstans i förenat kluster: Nej  
Utgång: 2025-09-12 05:00  
Format: Text  
Parametrar:  

***

#### 2. Movements dataset (CDB)

* Argument name: movements_dataset
* Path: "//UBUNTU1/share/Notforflyttningar.csv"
* Source: Rapportportalen

Details
Rubrik: Nötförflyttningar  
Dokumenttyp: Text  
Status Återkommande  
Destination: Filkopiera instansen med filnamnet: "Notforflyttningar.csv" till katalogen: "//UBUNTU1/share".  
Ägare: jonathan.udd  
Skapad: 2014-09-15 14:49  
Nästa körningstid: 2015-12-08 05:00  
Typ av upprepning: Objektet körs en gång var 1:e dag.  
Sökväg till överordnat objekt: SVA/PPN/  
Fjärrinstans i förenat kluster: Nej  
Utgång: 2024-08-22 13:32  
Format: Text  
Parametrar: 2013-01-01 00:00:00  

***

#### 3. SVASSS dataset

* Argument name: svasss_dataset
* Path: "//UBUNTU1/share/SVASSS.alarms.data.RData"
* Source: SVASSS (I:/ESS/SVASyndromicSurveillanceSystem)

***

#### 4. SJV dataset

* Argument name: sjv_dataset
* Path: "//UBUNTU1/share/sjv.data.RData"
* Source: SVASSS (I:/ESS/SVASyndromicSurveillanceSystem)

***

#### 5. Urax

* Argument name: urax_dataset
* Path:  "//UBUNTU1/share/urax.csv"
* Source: Rapportportalen

Details
Rubrik: URAX - Misstankar  
Dokumenttyp: Text  
Status Misslyckades  
Destination: Filkopiera instansen med filnamnet: "urax.txt" till katalogen: "//UBUNTU1/share".  
Ägare: giampaolo.cocca  
Skapad: 2015-12-07 05:00  
Starttid: 2015-12-07 05:01  
Sluttid: 2015-12-07 05:01  
Server som används: SVART.AdaptiveJobServer  
PID:  10084  
Sökväg till överordnat objekt: SVA/URAX/  
Fjärrinstans i förenat kluster: Nej  
Utgång: 2015-12-09 05:00  
Format: Text  
