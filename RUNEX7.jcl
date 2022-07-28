//D01      EXEC PGM=IEFBR14                      
//DEL01    DD DSN=[filepath].CMPOUT,      
//         DISP=(MOD,DELETE),SPACE=(TRK,0)    
//P10      EXEC PGM=EX7                          
//SYSUDUMP DD SYSOUT=*                           
//ABENDAID DD SYSOUT=*                           
//SYSOUT   DD SYSOUT=*                           
//IOMODOUT DD SYSOUT=*                           
//SYSABOUT DD SYSOUT=*                           
//EQADEBUG DD DSN=[filepath].SYSDEBUG,DISP=SHR
//         DD DSN=[filepath].SYSDEBUG,DISP=SHR
//         DD DSN=[filepath].LANGX,DISP=SHR   
//CMP1     DD DSN=[filepath].CMP1,        
//         DISP=SHR                              
//CMP2     DD DSN=[filepath].CMP2,        
//         DISP=SHR                              
//CMPOUT   DD DSN=[filepath].CMPOUT,      
//*        DISP=SHR,                        
//         DISP=(NEW,CATLG,DELETE),         
//         SPACE=(TRK,(50,1),RLSE),         
//         DCB=(RECFM=FB,LRECL=13,BLKSIZE=0)