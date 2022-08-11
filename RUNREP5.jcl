//NRSD149F JOB BAAI0000,
//            '&OADOWNER',
//             CLASS=S,
//             LINES=(999,WARNING),
//             MSGCLASS=R,
//             TIME=1440
//JOBLIB   DD DSN=[filepath].PHASELIB,
//            DISP=SHR
//         DD DSN=[filepath].LOADLIB,
//            DISP=SHR
//*-------------------------------------------------------------------*
//D01      EXEC PGM=IEFBR14
//DEL01    DD DSN=[filepath].REP2SORT,
//            DISP=(MOD,DELETE),SPACE=(TRK,0)
//*-------------------------------------------------------------------*
//P10      EXEC PGM=SORT
//SORTIN   DD DSN=[filepath].REP2IN,DISP=SHR
//SYSUDUMP DD SYSOUT=*
//SYSIN    DD *
  SORT FIELDS=(1,2,CH,A,
               3,2,CH,A,
               5,3,CH,A,
               8,4,CH,A,
               12,6,CH,A)
  SUM FIELDS=NONE
//SORTOUT  DD DSN=[filepath].REP2SORT,
//         DISP=(NEW,CATLG,DELETE),
//         SPACE=(TRK,(50,1),RLSE),
//         DCB=(RECFM=FB,LRECL=36,BLKSIZE=0)
//ABENDAID DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//IOMODOUT DD SYSOUT=*
//SYSABOUT DD SYSOUT=*
//EQADEBUG DD DSN=[filepath].SYSDEBUG,DISP=SHR
//         DD DSN=[filepath].SYSDEBUG,DISP=SHR
//         DD DSN=[filepath].LANGX,DISP=SHR
//*-------------------------------------------------------------------*
//P20      EXEC PGM=REPORT5
//REP5IN   DD DSN=[filepath].REP2SORT,DISP=SHR
//SYSUDUMP DD SYSOUT=*
//ABENDAID DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//IOMODOUT DD SYSOUT=*
//SYSABOUT DD SYSOUT=*
//EQADEBUG DD DSN=[filepath].SYSDEBUG,DISP=SHR
//         DD DSN=[filepath].SYSDEBUG,DISP=SHR
//         DD DSN=[filepath].LANGX,DISP=SHR