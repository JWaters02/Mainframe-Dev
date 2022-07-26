//D01      EXEC PGM=IEFBR14                       
//DEL01    DD DSN=[filepath].SORTOUT,      
//            DISP=(MOD,DELETE),SPACE=(TRK,0)     
//DEL02    DD DSN=[filepath].CDOUT,        
//            DISP=(MOD,DELETE),SPACE=(TRK,0)     
//P10      EXEC PGM=SORT                          
//SYSUDUMP DD SYSOUT=*                            
//ABENDAID DD SYSOUT=*                            
//SYSOUT   DD SYSOUT=*                            
//SORTIN   DD DSN=[filepath].INPUT,DISP=SHR
//IOMODOUT DD SYSOUT=*                            
//SYSABOUT DD SYSOUT=*                            
//EQADEBUG DD DSN=[filepath].SYSDEBUG,DISP=SHR 
//         DD DSN=[filepath].SYSDEBUG,DISP=SHR 
//         DD DSN=[filepath].LANGX,DISP=SHR    
//SYSIN    DD *                                   
    SORT FIELDS=(1,6,CH,A,                          
                 7,2,CH,A,                            
                 9,2,CH,A,                            
                 11,4,CH,A,                           
                 15,4,ZD,A)                           
    SUM FIELDS=(20,3,ZD)                              
//SORTOUT  DD DSN=[filepath].SORTOUT,        
//*        DISP=SHR,                                
//         DISP=(NEW,CATLG,DELETE),                 
//         SPACE=(TRK,(50,1),RLSE),                 
//         DCB=(RECFM=FB,LRECL=22,BLKSIZE=0)        
//***********************************************   
//P20      EXEC PGM=EX5                             
//SYSUDUMP DD SYSOUT=*                              
//ABENDAID DD SYSOUT=*                              
//SYSOUT   DD SYSOUT=*                              
//SORTOUT  DD DSN=[filepath].SORTOUT,DISP=SHR
//IOMODOUT DD SYSOUT=*                           
//SYSABOUT DD SYSOUT=*                           
//EQADEBUG DD DSN=[filepath].SYSDEBUG,DISP=SHR
//         DD DSN=[filepath].SYSDEBUG,DISP=SHR
//         DD DSN=[filepath].LANGX,DISP=SHR   
//CDOUT    DD DSN=[filepath].CDOUT,       
//*        DISP=SHR,                             
//         DISP=(NEW,CATLG,DELETE),              
//         SPACE=(TRK,(50,1),RLSE),              
//         DCB=(RECFM=FB,LRECL=22,BLKSIZE=0)     
//CEEOPTS  DD *                                  
    TEST(,,,TCPIP&[ip]%[port]:)             