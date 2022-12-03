
# DATA SCRAMBLING TOOL

<img src="https://github.com/Vlutsas/SAP_DST/blob/main/DST2.png"  width="400" />

* [Description](#Description)
* [Prerequisites](#Prerequisites)
* [Authorization](#Authorization)
* [Features](#Features)
* [Links](#Links)

### Description

The PHI/PII Data Scrambling Tool is intended to provide facilities for the irreversible anonymisation of PII/PHI data in non-PRD SAP systems, including (but not limited) to patient name, contact names, personal email addresses and personal bank account details.
> Installation requirements: <br>
> AS ABAP 7.40<br>
> <a href="https://docs.abapgit.org/">abapGit</a><br>

### Prerequisites

* Create custom Application log object ZDST (t-code SLG0).
* Create Background event DST_START (t-code SM64)
* Background mode requires creation of a Batch Job. Start Condition - After Event DST_START, with Periodic Job checkbox is set. 
* Create and assign a role with Z_S_DST authorization object and authorization group ZAG_DST

  Object  ‘Z_S_DST’;  
  Field name ‘ACTVT’;  
  ‘02’ - for template edit ; 
  ‘16’ – to execute scrambling

### Features

* Double-click on field “Key” opens a popup with a window contains am editable list of table fields (keys) for the table enter in “Table name” field in the same row. 
* Double click on table name displays data marked for scrambling (existing keys used for data selection).
* Scrambled data has the same data length as original data. 
* Application log contains info on app runs, brief info on scrambled data, template update, app run failed attempts etc.


### Links

* [ISO/IEC 27002:2013
Information technology — Security techniques — Code of practice for information security controls](https://www.iso.org/standard/54533.html)
* [Complete guide to GDPR compliance](https://gdpr.eu/)

<br>
<br>
<br>

