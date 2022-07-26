//D01      EXEC PGM=IEFBR14                      
//DEL01    DD DSN=[filepath].BRCHOUT,     
//            DISP=(MOD,DELETE),SPACE=(TRK,0)    
//P10      EXEC PGM=EX6                          
//SYSUDUMP DD SYSOUT=*                           
//ABENDAID DD SYSOUT=*                           
//SYSOUT   DD SYSOUT=*                           
//IOMODOUT DD SYSOUT=*                           
//SYSABOUT DD SYSOUT=*                           
//EQADEBUG DD DSN=[filepath].SYSDEBUG,DISP=SHR
//         DD DSN=[filepath].SYSDEBUG,DISP=SHR
//         DD DSN=[filepath].LANGX,DISP=SHR   
//SORTOUT  DD DSN=[filepath].SORTOUT,     
//         DISP=SHR                              
//BRANCHES DD DSN=[filepath].BRCH,        
//         DISP=SHR                              
//BRCHOUT  DD DSN=[filepath].BRCHOUT,     
//*        DISP=SHR,                        
//         DISP=(NEW,CATLG,DELETE),         
//         SPACE=(TRK,(50,1),RLSE),         
//         DCB=(RECFM=FB,LRECL=42,BLKSIZE=0)