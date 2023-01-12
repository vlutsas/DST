*&---------------------------------------------------------------------*
*& Report  zcf_data_scramble_tool
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&                     A D M I N I S T R A T I O N                     *
*&---------------------------------------------------------------------*
* Created by   : Vladimir Lutsas
* Created on   : 21.07.2020 13:45:19
* Program Type : Utility
* Description  : DST
*                PHI/PII Data Scrambling tool
*
*&---------------------------------------------------------------------*
*&                    C H A N G E  H I S T O R Y                       *
*&---------------------------------------------------------------------*

REPORT zcfr_dst.

TYPE-POOLS: vrm, slis, icon.

INCLUDE rddkorri.
INCLUDE rddsobji.
INCLUDE <cl_alv_control>.
INCLUDE zcf_dst_cls.
INCLUDE zcf_dst_pboo01.

START-OF-SELECTION.

  lcl_dst_app=>run( ).
