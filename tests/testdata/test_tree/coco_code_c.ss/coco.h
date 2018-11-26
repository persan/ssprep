//=============================================================================
// Copyright (C)  2000 CelsiusTech Systems AB, SWEDEN
//=============================================================================
//
//  UNIT NAME
//          coco.h
//
//  ABSTRACT
//          Header file for the declararions in COCO's CAPI interface.
//
//  CONTENTS
//
//      Creation information:
//          Programmer      : Leif Ekeroot / LEEK
//          Creation date   : 2000-04-17
//          Department      : CTN
//          Project         : SS2000 Mk3E
//
//      Revision history:
//          1.0/LEEK/2000-04-17/Created.
//          1.1/LEEK/2000-05-16/Removed prefix "CAPI" and other corrections
//  051116 HESU  Added Coco_Geocentric. Removed returncode COCO_NOT_SUPPORTED.
//               Added missing Datum identifiers.
//               Added Geocentric.
//
//     Dependencies:
//           None.
//
//     Improvements:
//           No suggestions
//
//=============================================================================
#ifndef COCO_H
#define COCO_H

///////////////////////////////////////////////////////////////////////////////
//
// CONSTANTS AND TYPE DEFINITIONS
//
///////////////////////////////////////////////////////////////////////////////

//
// Return codes
//
#define COCO_SUCCESS 0
#define COCO_UNEXPECTED_EXCEPTION 1
#define COCO_INVALID_DATUM_IDENTIFIER 3

//
// External Datum Identifier codes
//
#define COCO_RT_38 0
#define COCO_ED_50 1
#define COCO_WGS_72 2
#define COCO_WGS_84 3
#define COCO_RT_90 4
#define COCO_AGD_66 5
#define COCO_AGD_84 6
#define COCO_GDA_94 7

//
// The position types supported by COCO Capi Interface
//
typedef struct {
    float x;
    float y;
    float z;
} CocoCartesian;

typedef struct {
    float dist;     // Distance, meter 
    float elev;     // Elevation, radian
    float bearing;  // Bearing, radian
} CocoPolar; 
     
#define COCO_LENGTH_OF_GEODETIC_DATA 18
typedef struct {
    char dummy[COCO_LENGTH_OF_GEODETIC_DATA];
} CocoGeodetic;
// This is a private type.

typedef struct {
    double latitude;
    double longitude;
} CocoGeographic;

typedef struct {
    double x;
    double y;
} CocoRt;

typedef struct {
    double x;
    double y;
    int external_identifier;
} CocoRtExtended;

typedef struct {
    double latitude_part;
    double longitude_part;
    int external_identifier;
} CocoExternalGeographic;

typedef struct {
    double x;
    double y;
    double z;
    int external_identifier;
} CocoGeocentric;


///////////////////////////////////////////////////////////////////////////////
//
// OPERATIONS
//
///////////////////////////////////////////////////////////////////////////////
#ifdef __cplusplus
extern "C" {
#endif

//
// Coco_Capi_Geodetic_To_Geographic
//
// Converts between geodetic and geographic.
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//
extern void Coco_Capi_Geodetic_To_Geographic 
           (CocoGeodetic *geodetic_position,     // in
            CocoGeographic *geographic_position, // out
            int *return_code);                   // out


//
// Coco_Capi_Geographic_To_Geodetic
//
// Converts between geodetic and geographic.
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//
extern void Coco_Capi_Geographic_To_Geodetic 
           (CocoGeographic *geographic_position,  // in
            CocoGeodetic *geodetic_position,      // out
            int *return_code);                    // out


//
// Coco_Capi_Geodetic_To_Rt
//
// Converts between geodetic and rt.
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//
extern void Coco_Capi_Geodetic_To_Rt 
           (CocoGeodetic *geodetic_position,  // in
            CocoRt *rt_position,              // out
            int *return_code);                // out


//
// Coco_Capi_Rt_To_Geodetic
//
// Converts between rt and geodetic.
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//
extern void Coco_Capi_Rt_To_Geodetic 
           (CocoRt *rt_position,              // in
            CocoGeodetic *geodetic_position,  // out
            int *return_code);                // out

//
// Coco_Capi_Geodetic_To_Rt_Extended
//
// Converts between geodetic and rt extended.
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//     COCO_INVALID_DATUM_IDENTIFIER
//
extern void Coco_Capi_Geodetic_To_Rt_Extended 
           (CocoGeodetic *geodetic_position,  // in
            CocoRtExtended *rt_position,      // out
            int *return_code);                // out


//
// Coco_Capi_Rt_Extended_To_Geodetic
//
// Converts between extended rt and geodetic.
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//     COCO_INVALID_DATUM_IDENTIFIER
//
extern void Coco_Capi_Rt_Extended_To_Geodetic 
           (CocoRtExtended *rt_position,      // in
            CocoGeodetic *geodetic_position,  // out
            int *return_code);                // out

//
// Coco_Capi_External_Geographic_To_Rt
//
// Converts from external geographic to RT
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//     COCO_INVALID_DATUM_IDENTIFIER
//
extern void Coco_Capi_External_Geographic_To_Rt  
           (CocoExternalGeographic *external_geographic_position,  // in
            CocoRtExtended *rt_position,                           // out
            int *return_code);                                     // out

//
// Coco_Capi_Rt_To_External_Geographic
//
// Converts from RT to external geographic
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//     COCO_INVALID_DATUM_IDENTIFIER
//
extern void Coco_Capi_Rt_To_External_Geographic  
           (CocoRtExtended *rt_position,                           // in
            CocoExternalGeographic *external_geographic_position,  // out
            int *return_code);                                     // out

//
// Coco_Capi_External_Geographic_To_Geodetic
//
// Converts from external geographic to geodetic
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//     COCO_INVALID_DATUM_IDENTIFIER
//
extern void Coco_Capi_External_Geographic_To_Geodetic
           (CocoExternalGeographic *external_geographic_position,  // in
            CocoGeodetic *geodetic_position,                       // out
            int *return_code);                                     // out

//
// Coco_Capi_Geodetic_To_External_Geographic
//
// Converts from geodetic to external geographic
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//     COCO_INVALID_DATUM_IDENTIFIER
//
extern void Coco_Capi_Geodetic_To_External_Geographic
		   (CocoGeodetic *geodetic_position,                       // in
            int external_identity,								   // in
            CocoExternalGeographic *external_geographic_position,  // out
            int *return_code);                                     // out

//
// Coco_Capi_Geocentric_To_Geodetic
//
// Converts from geocentric to geodetic
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//     COCO_INVALID_DATUM_IDENTIFIER
//
extern void Coco_Capi_Geocentric_To_Geodetic
           (CocoGeocentric *geocentric_position,                   // in
            CocoGeodetic *geodetic_position,                       // out
            int *return_code);                                     // out

//
// Coco_Capi_Geodetic_To_Geocentric
//
// Converts from geodetic to geocentric
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//     COCO_INVALID_DATUM_IDENTIFIER
//
extern void Coco_Capi_Geodetic_To_Geocentric
		   (CocoGeodetic *geodetic_position,                       // in
            int external_identity,								   // in
            CocoGeocentric *geocentric_position,                   // out
            int *return_code);                                     // out

//
// Coco_Capi_Geocentric_Position_Transform
//
// Transforms (datum shift) a Geocentric position
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//     COCO_INVALID_DATUM_IDENTIFIER
//
extern void Coco_Capi_Geocentric_Position_Transform
           (CocoGeocentric *geocentric_position,                   // in
            int new_external_identity,							   // in
            CocoGeodetic *new_geocentric_position,                 // out
            int *return_code);                                     // out


//
// Coco_Capi_Great_Circle_Position_Of
//
// Calculates the position at Azimuth/Distance from reference_position.
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//
extern void Coco_Capi_Great_Circle_Position_Of 
           (float *Azimuth,                    // in 
            float *Distance,                   // in
            CocoGeodetic *reference_position,  // in
            CocoGeodetic *position,            // out
            int *return_code);                 // out 

//
// Coco_Capi_Great_Circle_Polar
//
// Computes the distance between two geodetic positions and
// the north oriented azimuth from reference_position to
// to_referred_position
//
// Possible return codes are:
//
//     COCO_SUCCESS
//     COCO_UNEXPECTED_EXCEPTION
//
extern void Coco_Capi_Great_Circle_Polar 
       (CocoGeodetic *reference_position,    // in
        CocoGeodetic *to_referred_position,  // in
        float *Distance,                     // out
        float *Azimuth,                      // out
        int *return_code);                   // out

#ifdef __cplusplus
}
#endif
#endif // COCO_H