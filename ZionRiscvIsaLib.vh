////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Copyright(C) Zion Team. Open source License: MIT.
// ALL RIGHT RESERVED
// Filename : ZionRiscvIsaLib.vh
// Author   : Zion Team
// Date     : 2019-06-20
// Version  : 0.1
// Description :
//     This is a header file of basic circuits for RISCV ISA, including execution circuits and decode circuits. 
// Modification History:
//   Date   |   Author    |   Version   |   Change Description
//======================================================================================================================
// 19-10-22 |  Zion Team  |     0.1     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////




`define ZionRiscvIsaLib_MacroDef(ImportName, DefName)                     \
  `ifdef ImportName``DefName                                              \
    Macro Define Error: ImportName``DefName has already been defined!!    \
  `else                                                                   \
    `define ImportName``DefName `ZionRiscvIsaLib_``DefName                \
  `endif
`define ZionRiscvIsaLib_PackageDef(ImportName, DefName)                   \
  `ifdef ImportName``DefName                                              \
    Macro Define Error: ImportName``DefName has already been defined!!    \
  `else                                                                   \
    `define ImportName``DefName ZionRiscvIsaLib_``DefName                 \
  `endif
`define ZionRiscvIsaLib_InterfaceDef(ImportName, DefName)                 \
  `ifdef ImportName``DefName                                              \
    Macro Define Error: ImportName``DefName has already been defined!!    \
  `else                                                                   \
    `define ImportName``DefName ZionRiscvIsaLib_``DefName                 \
  `endif
`define ZionRiscvIsaLib_ModuleDef(ImportName, DefName)                    \
  `ifdef ImportName``DefName                                              \
    Macro Define Error: ImportName``DefName has already been defined!!    \
  `else                                                                   \
    `define ImportName``DefName `ZionRiscvIsaLib_``DefName                \
  `endif
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

`define Use_ZionRiscvIsaLib(ImportName)                                   \
  `ZionRiscvIsaLib_InterfaceDef(ImportName,RvimazDecodeItf)               \
  `ZionRiscvIsaLib_InterfaceDef(ImportName,AddSubExItf)                   \
  `ZionRiscvIsaLib_ModuleDef(ImportName, AddSubExec)                      \
  `ZionRiscvIsaLib_ModuleDef(ImportName, AddSubLessThan)                  \
  `ZionRiscvIsaLib_InterfaceDef(ImportName,BitsExItf)                     \
  `ZionRiscvIsaLib_ModuleDef(ImportName, BitsOpExec)                      \
  `ZionRiscvIsaLib_InterfaceDef(ImportName,BjExItf)                       \
  `ZionRiscvIsaLib_ModuleDef(ImportName, BjTgtAddr)                       \
  `ZionRiscvIsaLib_ModuleDef(ImportName, BjEnNoLt)                        \
  `ZionRiscvIsaLib_ModuleDef(ImportName, BjEnStandby)                     \
  `ZionRiscvIsaLib_ModuleDef(ImportName, JumpLinkPc)                      \
  `ZionRiscvIsaLib_InterfaceDef(ImportName,SftExItf)                      \
  `ZionRiscvIsaLib_ModuleDef(ImportName, SftExec)                         \
  `ZionRiscvIsaLib_InterfaceDef(ImportName,SltExItf)                      \
  `ZionRiscvIsaLib_ModuleDef(ImportName, SetLessThan)                     \
  `ZionRiscvIsaLib_InterfaceDef(ImportName,LoadExItf)                     \
  `ZionRiscvIsaLib_InterfaceDef(ImportName,StoreExItf)                    \
  `ZionRiscvIsaLib_InterfaceDef(ImportName,IntInsExItf)                   \
  `ZionRiscvIsaLib_ModuleDef(ImportName, IntEx)                           

`define Unuse_ZionRiscvIsaLib(ImportName)                                 \
  `undef ImportName``RvimazDecodeItf                                      \
  `undef ImportName``AddSubExItf                                          \   
  `undef ImportName``AddSubExec                                           \   
  `undef ImportName``AddSubLessThan                                       \   
  `undef ImportName``BitsExItf                                            \   
  `undef ImportName``BitsOpExec                                           \   
  `undef ImportName``BjExItf                                              \   
  `undef ImportName``BjTgtAddr                                            \   
  `undef ImportName``BjEnNoLt                                             \   
  `undef ImportName``BjEnStandby                                          \   
  `undef ImportName``JumpLinkPc                                           \   
  `undef ImportName``SftExItf                                             \   
  `undef ImportName``SftExec                                              \   
  `undef ImportName``SltExItf                                             \   
  `undef ImportName``SetLessThan                                          \   
  `undef ImportName``LoadExItf                                            \   
  `undef ImportName``StoreExItf                                           \   
  `undef ImportName``IntInsExItf                                          \   
  `undef ImportName``IntEx                                                
              

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////






