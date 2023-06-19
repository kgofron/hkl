#ifndef _XRAYS_IMAGE_SPE_H
#define _XRAYS_IMAGE_SPE_H

#include <stdio.h>

#include "xrays-image.h"

XRAYS_BEGIN_DECLS

#pragma pack(1)

typedef struct {
	unsigned short int	dioden;			//    0 num of physical pixels (X axis)    
	short int		avgexp;			//    2 number of accumulations per scan   
	//      if > 32767, set to -1 and        
	//      see lavgexp below (668)          
	short int		exposure;		//    4 exposure time (in milliseconds)    
	//      if > 32767, set to -1 and        
	unsigned short int	xDimDet;      		//    6 Detector x dimension of chip       
	short int          	mode;        	 	//    8 timing mode                        
	float        		exp_sec;      		//   10 alternative exposure, in secs.     
	short int          	asyavg;       		//   14 number of asynchron averages       
	short int          	asyseq;      	 	//   16 number of asynchron sequential     
	unsigned short int 	yDimDet;     	 	//   18 y dimension of CCD or detector.    
	char         		date[10];    	 	//   20 date as MM/DD/YY                   
	short int          	ehour;       	 	//   30 Experiment Time: Hours (as binary) 
	short int          	eminute;     	 	//   32 Experiment Time: Minutes(as binary)
	short int          	noscan;      	 	//   34 number of multiple scans           
	//    if noscan == -1 use lnoscan        
	short int          	fastacc;      		//   36                                    
	short int          	seconds;      		//   38 Experiment Time: Seconds(as binary)
	short int          	DetType;      		//   40 CCD/DiodeArray type                
	unsigned short int 	xdim;         		//   42 actual # of pixels on x axis       
	short int          	stdiode;      		//   44 trigger diode                      
	float       		nanox;        		//   46                                    
	float       		calibdio[10]; 		//   50 calibration diodes                 
	char         		fastfile[16];     	//   90 name of pixel control file          
	short int          	asynen;           	//  106 asynchron enable flag 0 = off       
	short int          	datatype;         	//  108 experiment data type                
	//         0=    FLOATING POINT             
	//         1=    LONG INTEGER               
	//         2=    INTEGER                    
	//         3=    UNSIGNED INTEGER           
	float        		calibnan[10];     	//  110 calibration nanometer               
	short int          	BackGrndApplied;  	//  150 set to 1 if background sub done     
	short int          	astdiode;         	//  152                                     
	unsigned short int 	minblk;           	//  154 min. # of strips per skips          
	unsigned short int 	numminblk;        	//  156 # of min-blocks before geo skps     
	double       		calibpol[4];      	//  158 calibration coefficients            
	unsigned short int 	ADCrate;          	//  190 ADC rate                            
	unsigned short int 	ADCtype;          	//  192 ADC type                            
	unsigned short int 	ADCresolution;    	//  194 ADC resolution                      
	unsigned short int 	ADCbitAdjust;     	//  196 ADC bit adjust                      
	unsigned short int 	gain;             	//  198 gain                                
	char         		exprem[5][80];    	//  200 experiment remarks                  
	unsigned short int 	geometric;        	//  600 geometric operations rotate 0x01    
	//       reverse 0x02, flip 0x04            
	char         		xlabel[16];       	//  602 Intensity display string            
	unsigned short int 	cleans;           	//  618 cleans                              
	unsigned short int 	NumSkpPerCln;     	//  620 number of skips per clean.          
	char         		califile[16];     	//  622 calibration file name (CSMA)        
	char         		bkgdfile[16];     	//  638 background file name                
	short int          	srccmp;           	//  654 number of source comp. diodes       
	unsigned short int 	ydim;             	//  656 y dimension of raw data.            
	short int          	scramble;         	//  658 0 = scrambled, 1 = unscrambled      
	long         		lexpos;           	//  660 long exposure in milliseconds       
	//         used if exposure set to -1       
	long         		lnoscan;          	//  664 long num of scans                   
	//         used if noscan set to -1         
	long         		lavgexp;          	//  668 long num of accumulations           
	//         used if avgexp set to -1         
	char         		stripfil[16];     	//  672 stripe file (st130)                 
	char         		version[16];      	//  688 version & date:"01.000 02/01/90"    
	short int          	type;             	//  704   1 = new120 (Type II)              
	//        2 = old120 (Type I )              
	//        3 = ST130                         
	//        4 = ST121                         
	//        5 = ST138                         
	//        6 = DC131 (PentaMAX)              
	//        7 = ST133 (MicroMAX/SpectroMax),  
	//        8 = ST135 (GPIB)                  
	//        9 = VICCD                         
	//       10 = ST116 (GPIB)                  
	//       11 = OMA3 (GPIB)                   
	//       12 = OMA4                          
	short int          	flatFieldApplied;	//  706 Set to 1 if flat field was applied  
	short int          	spare[8];         	//  708 reserved                            
	short int          	kin_trig_mode;     	//  724 Kinetics Trigger Mode               
	char         		empty[702];       	//  726 EMPTY BLOCK FOR EXPANSION           
	float        		clkspd_us;        	// 1428 Vert Clock Speed in micro-sec       
	short int         	HWaccumFlag;      	// 1432 set to 1 if accum done by Hardware  
	short int          	StoreSync;        	// 1434 set to 1 if store sync used.        
	short int          	BlemishApplied;   	// 1436 set to 1 if blemish removal applied 
	short int          	CosmicApplied;    	// 1438 set to 1 if cosmic ray removal done 
	short int          	CosmicType;       	// 1440 if cosmic ray applied, this is type 
	float        		CosmicThreshold;  	// 1442 Threshold of cosmic ray removal.    
	long         		NumFrames;        	// 1446 number of frames in file.           
	float        		MaxIntensity;     	// 1450 max short intensity of data (future)      
	float        		MinIntensity;     	// 1454 min short intensity of data (future)      
	char         		ylabel[16]; 		// 1458 y axis label.                       
	unsigned short int 	ShutterType;      	// 1474 shutter type.                       
	float        		shutterComp;      	// 1476 shutter compensation time.          
	unsigned short int 	readoutMode;      	// 1480 Readout mode, full, kinetics, etc.  
	unsigned short int 	WindowSize;       	// 1482 window size for kinetics only.     
	unsigned short int 	clkspd;           	// 1484 clock speed for kinetics &         
	//      frame transfer.                    
	unsigned short int 	interface_type;   	// 1486 computer short interface (isa-taxi,      
	//      pci, eisa, etc.)                   
	unsigned long 		ioAdd1;          	// 1488 I/O address of short interface card.     
	unsigned long 		ioAdd2;          	// 1492 if more than one address for card. 
	unsigned long 		ioAdd3;          	// 1496                                    
	unsigned short int  	intLevel;        	// 1500 short interrupt level short interface card     
	unsigned short int  	GPIBadd;         	// 1502 GPIB address (if used)             
	unsigned short int  	ControlAdd;      	// 1504 GPIB controller address (if used)  
	unsigned short int  	controllerNum;   	// 1506 if multiple controller system will 
	//       have controller # data came from. 
	//       (Future Item)                     
	unsigned short int 	SWmade;           	// 1508 Software which created this file   
	short int           	NumROI;          	// 1510 number of ROIs used. if 0 assume 1 
	// 1512 - 1630 ROI information             
	struct ROIinfo {               			//                                         
		unsigned short int startx;          	// left x start value.                     
		unsigned short int endx;            	// right x value.                          
		unsigned short int groupx;          	// amount x is binned/grouped in hw.       
		unsigned short int starty;          	// top y start value.                      
		unsigned short int endy;            	// bottom y value.                         
		unsigned short int groupy;          	// amount y is binned/grouped in hw.       
	} ROIinfoblk[10];              			//    ROI Starting Offsets:                
	//            ROI 1 = 1512                 
	//            ROI 2 = 1524                 
	//            ROI 3 = 1536                 
	//            ROI 4 = 1548                 
	//            ROI 5 = 1560                 
	//            ROI 6 = 1572                 
	//            ROI 7 = 1584                 
	//            ROI 8 = 1596                 
	//            ROI 9 = 1608                 
	//            ROI 10 = 1620                
	char			FlatField[120];  	// 1632 Flat field file name.              
	char          		background[120]; 	// 1752 Background sub. file name.         
	char          		blemish[120];    	// 1872 Blemish file name.                 
	float         		software_ver;    	// 1992 Software version.                  
	char          		UserInfo[1000];  	// 1996-2995 user data.                    
	long          		WinView_id;      	// 2996 Set to 0x01234567L if file was     
	//      created by WinX                    
	struct xCalibration {
		double 		offset;        		// 3000 offset for absolute data scaling 
		double 		factor;        		// 3008 factor for absolute data scaling 
		char   		current_unit;  		// 3016 selected scaling unit            
		char   		reserved1;     		// 3017 reserved                         
		char   		string[40];    		// 3018 special string for scaling       
		char   		reserved2[40]; 		// 3058 reserved                         
		char   		calib_valid;   		// 3098 flag if calibration is valid     
		char   		input_unit;    		// 3099 current input units for          
		//      "calib_value"                    
		char   		polynom_unit;  		// 3100 linear UNIT and used             
		//      in the "polynom_coeff"           
		char          	polynom_order;     	// 3101 ORDER of calibration POLYNOM     
		char          	calib_count;       	// 3102 valid calibration data pairs     
		double        	pixel_position[10];	// 3103 pixel pos. of calibration data   
		double        	calib_value[10];   	// 3183 calibration VALUE at above pos   
		double        	polynom_coeff[6]; 	// 3263 polynom COEFFICIENTS             
		double        	laser_position;    	// 3311 laser wavenumber for relativ WN  
		char          	reserved3;         	// 3319 reserved                         
		unsigned char 	new_calib_flag;    	// 3320 If set to 200, valid label below 
		char          	calib_label[81];   	// 3321 Calibration label (NULL term'd)  
		char          	expansion[87];     	// 3402 Calibration Expansion area       
	} xcalibration;
	struct yCalibration {
		double        	offset;            	// 3489 offset for absolute data scaling 
		double        	factor;            	// 3497 factor for absolute data scaling 
		char          	current_unit;      	// 3505 selected scaling unit            
		char          	reserved1;         	// 3506 reserved                         
		char          	string[40];        	// 3507 special string for scaling       
		char          	reserved2[40];     	// 3547 reserved                         
		char          	calib_valid;       	// 3587 flag if calibration is valid     
		char          	input_unit;        	// 3588 current input units for          
		//      "calib_value"                    
		char          	polynom_unit;      	// 3589 linear UNIT and used             
		//      in the "polynom_coeff"           
		char          	polynom_order;     	// 3590 ORDER of calibration POLYNOM     
		char          	calib_count;       	// 3591 valid calibration data pairs     
		double        	pixel_position[10];	// 3592 pixel pos. of calibration data   
		double        	calib_value[10];   	// 3672 calibration VALUE at above pos   
		double        	polynom_coeff[6]; 	// 3752 polynom COEFFICIENTS             
		double        	laser_position;    	// 3800 laser wavenumber for relativ WN  
		char          	reserved3;         	// 3808 reserved                         
		unsigned char 	new_calib_flag;    	// 3809 If set to 200, valid label below 
		char          	calib_label[81];   	// 3810 Calibration label (NULL term'd)  
		char          	expansion[87];     	// 3891 Calibration Expansion area       
	} ycalibration;
	char 			Istring[40]; 		// 3978 special Intensity scaling string    
	char 			empty3[80];  		// 4018 empty block to reach 4100 bytes     
	short int  		lastvalue;   		// 4098 Always the LAST value in the header 
} WINXHEAD;

#pragma pack()

XRaysImage* xrays_image_spe_read(FILE *file);

void xrays_image_spe_write(XRaysImage *img, FILE *file);

XRAYS_END_DECLS

#endif
