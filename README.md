# Mainframe-Dev

## Objective 1:
Copy and paste some random file and see how it works.

## Objective 2:
Practice Cobol syntax and logical thinking to use repeating statements (loop). 

Code a program that displays on sysout: 
- “Hello World”
- “Numbers from 1 to 100: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10…”
- “Even Numbers from 1 to 100: 2, 4, 6, 8, 10…”
- “Odd Numbers from 1 to 100: 1, 3, 5, 7, 9…”


## Objective 3:
Practice Cobol syntax and logical thinking to use conditional and repetitive statements to process an input file.

Make a copy of the file [filepath].INPUT as [filepath][id].INPUT and use it as input. Make sure this file has records with the same key: ITEM + OPTION + PARTNER CODE.

Code a program that reads an input file with the layout described below and displays all records with field OPTION equal to ‘01’, ‘03’, ‘04’ or ‘05’ on the sysout. In addition, after all records are processed, display counters:
- How many records were read?
- How many records were eligible to be displayed on sysout?

File layout:
Field | Type and length
-----|--------------------
ITEM | Alphanumeric 6 bytes
OPTION | Alphanumeric 2 bytes
PARTNER CODE | Alphanumeric 2 bytes
BRANCH | Alphanumeric 4 bytes
PRICE | Numeric 5.2
QUANTITY | Numeric 3


## Objective 4:
Create and sort input file and practice Cobol syntax and logical thinking to group records.

Using the input file created in exercise 2, create a JCL SORT to order the file by ITEM + OPTION. Code a program to read this file sorted by ITEM + OPTION and display on sysout how many records per item were read. 

## Objective 5:
Create and sort sum input file and practice Cobol syntax and logical thinking to write an output file from an input file. 

Using the input file created in exercise 2, create a JCL SORT group records by ITEM + OPTION + PARTNER CODE + BRANCH + PRICE and sum field QUANTITY up. Code a program to read this file and write an output file with the input records where quantity > 100. 
Display on sysout the counters below: 
- How many records were read
- How many records were written

## Objective 6:
Use an internal table to help create output file.  

Create file BRANCH with the layout below:

Field | Type and length
-----|--------------------
BRANCH | Alphanumeric 4 bytes
DESCRIPTION | Alphanumeric 30 bytes

Using file created in exercise 4, code a program that will read this file and write an output with the layout below: 
Field | Type and length
-----|--------------------
ITEM | Alphanumeric 6 bytes
OPTION | Alphanumeric 2 bytes
BRANCH | Alphanumeric 4 bytes
BRANCH DESCRIPTION | Alphanumeric 30 bytes

Where BRANCH DESCRIPTION must be obtained from file BRANCH using the input field BRANCH (from file created in exercise 4) as searching key.


## Objective 7:
Compare files and create output (2 file match).

File 1 layout:
Field | Type and length
-----|--------------------
ITEM | Alphanumeric 6 bytes

File 2 layout:
Field | Type and length
-----|--------------------
ITEM | Alphanumeric 6 bytes
OPTION | Alphanumeric 2 bytes
PRICE | Numeric 5.2

2 File Match: read 2 input files comparing/matching the key field ITEM. When there is a match, read all OPTIONS in file 2 and write an output file. In addition, write the ITEM found only in file 1 and the ones found only in file 2.

Output file layouts:
- Records with matched key (File 2 layout)
- Records found only in input file 1 (File 1 layout)
- Records found only in input file 2 (File 2 layout)
