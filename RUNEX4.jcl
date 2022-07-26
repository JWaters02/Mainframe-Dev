//D01      EXEC PGM=IEFBR14                       
//DEL01    DD DSN=[filepath].SORTOUT,      
//         DISP=(MOD,DELETE),SPACE=(TRK,0)     
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
    SORT FIELDS=(1,6,CH,A,7,8,CH,A)                 
//SORTOUT  DD DSN=[filepath].SORTOUT,        
//*        DISP=SHR,                                
//         DISP=(NEW,CATLG,DELETE),                 
//         SPACE=(TRK,(50,1),RLSE)                  
//***********************************************   
//P20      EXEC PGM=EX4                             
//SYSUDUMP DD SYSOUT=*                              
//ABENDAID DD SYSOUT=*                              
//SYSOUT   DD SYSOUT=*                              
//SORTOUT  DD DSN=[filepath].SORTOUT,DISP=SHR
//IOMODOUT DD SYSOUT=*                              
//SYSABOUT DD SYSOUT=*                              
//EQADEBUG DD DSN=[filepath].SYSDEBUG,DISP=SHR   
//         DD DSN=[filepath].SYSDEBUG,DISP=SHR   
//         DD DSN=[filepath].LANGX,DISP=SHR      