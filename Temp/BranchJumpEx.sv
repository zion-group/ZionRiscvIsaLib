
//section: BranchJumpEx ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Circuits about Branch and Jump instructions are provided in this section.
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interface name : ZionRiscvIsaLib_BjExItf
// Author         : Wenheng Ma
// Date           : 2019-10-27
// Version        : 1.0
// Parameter      :
//   RV64 - indicate whether the circuit is for RV64 or not. 1:RV64.  2:RV32. It can be inferred from iSftExIf.
// Description    :
//   Define signals that branch and jump nead. And offer an Excution function to help getting the result.
//   Details for each function is described below:
//     TgtAddrGen      : Calculate the target address for branch and jump instructions.
//     LessThan        : Calculate less than operation for BLT and BGE instructions.
//     LinkPcGen       : Calculate link PC for jump instructions. 
//                       It could be omitted, if reuse the adder in ADD instruction.
//     S1LinkOffsetMux : To reuse adder for link PC calculation, s1 must be link offset instead of normal s1.
//                       This fuction implement a high-performance mux.
//   Parameter RV64 indicate whether the processor is 64-bit core with the ISA of RV64I.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-27 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
`ifndef Disable_ZionRiscvIsaLib_BjExItf
interface ZionRiscvIsaLib_BjExItf
#(RV64 = 0);

  localparam CPU_WIDTH = 32*(RV64+1);
  logic bjEn, branch, beq, bne, blt, bge, unsignedFlg, jump;
  logic [1:0] linkOffset;
  logic [CPU_WIDTH-1:0] pc, s1, s2, offset, tgtAddr, linkPc, s1Fnl;

  // Calculate branch&jump target address.
  // For Jal instruction, the PC must put into s1 before execution.
  function automatic logic [CPU_WIDTH-1:0] TgtAddrGen;

    logic [CPU_WIDTH-1:0] baseAddr, tgtOffset;
    baseAddr  =  ({$bits(pc){branch}} & pc)  // TODO: use Mask
                |({$bits(s1){ jump }} & s1); // TODO: use Mask
    tgtOffset = {$bits(offset){bjEn}} & offset;  // TODO: use Mask
    return (baseAddr + tgtOffset);

  endfunction : TgtAddrGen

  // Calculate less than for Blt Bge instructions.
  // Extend data according to unsigned flag(unsignedFlg), then compare the size of s1 and s2.
  // With the extension of data, we can reuse the compare circuits for both signed and unsigned compare operations.
  function automatic logic LessThan;

    logic signed [CPU_WIDTH:0] s1Extd, s1Mask, s2Extd, s2Mask;
    s1Extd = {((~unsignedFlg) & s1[CPU_WIDTH-1]) , s1}; // Extend s1 according unsignedFld // TODO: use HighBit
    s2Extd = {((~unsignedFlg) & s2[CPU_WIDTH-1]) , s2}; // Extend s2 according unsignedFld// TODO: use HighBit
    s1Mask = {$bits(s1Extd){branch}} & s1Extd; // TODO: use Mask
    s2Mask = {$bits(s2Extd){branch}} & s2Extd; // TODO: use Mask
    return ((s1Mask<s2Mask)? 1'b1:1'b0);

  endfunction : LessThan

  // Calculate the link pc. 
  function automatic logic [CPU_WIDTH-1:0] LinkPcGen;
    return (({$bits(pc){jump}}&pc) + ({linkOffset,1'b0}}); // TODO: use Mask
  endfunction : LinkPcGen

  // To reuse adder for generating link pc, the function works as a mux that selects s1 or link offset.
  // But it is implemented by 'bit or' for high bits. So it has a better performance.
  function automatic logic [CPU_WIDTH-1:0] S1LinkOffsetMux;
    logic [CPU_WIDTH-1:0] s1Mask, muxRslt;
    s1Mask = (~(s1|jump));
    muxRslt[CPU_WIDTH-1:3] = s1Mask[CPU_WIDTH-1:3];
    muxRslt[2:1] = s1Mask[2:1] | linkOffset;
    muxRslt[ 0 ] = s1Mask[ 0 ];
    return muxRslt;
  endfunction : S1LinkOffsetMux

  modport De( output bjEn, branch, beq, bne, blt, bge, unsignedFlg, jump, pc, s1, s2, offset);
  modport Ex( input  bjEn, branch, beq, bne, blt, bge, unsignedFlg, jump, pc, s1, s2, offset,
              import LessThan
            );
  modport BjTgtAddrOut (output tgtAddr);
  modport JumpLinkPcOut(output linkPc );
  modport S1MuxOut(output s1Fnl);
endinterface : ZionRiscvIsaLib_BjExItf
`endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module name : ZionRiscvIsaLib_BjTgtAddr
// Author      : Wenheng Ma
// Date        : 2019-10-28
// Version     : 1.0
// Parameter   : None
// Description :
//   Target address generator for branch and jump instructions.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-28 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

`ifndef Disable_ZionRiscvIsaLib_BjTgtAddr
`ifdef ZionRiscvIsaLib_BjTgtAddr
  `__DefErr__(ZionRiscvIsaLib_BjTgtAddr)
`else
  `define ZionRiscvIsaLib_BjTgtAddr(UnitName,iBjExIf_MT,oTgtAddrIf_MT) \
ZionRiscvIsaLib_BjTgtAddr UnitName(                                    \
                              .iBjExIf(iBjExIf_MT),                    \
                              .oTgtAddrIf(oTgtAddrIf_MT)               \
                            )
`endif
module ZionRiscvIsaLib_BjTgtAddr
(
  ZionRiscvIsaLib_BjExItf iBjExIf,
  ZionRiscvIsaLib_BjExItf.BjTgtAddrOut oTgtAddrIf
);

  always_comb begin
    oTgtAddrIf.tgtAddr  = iBjExIf.TgtAddrGen();
  end

endmodule: ZionRiscvIsaLib_BjTgtAddr
`endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module name : ZionRiscvIsaLib_BjEnNoLessThan
// Author      : Wenheng Ma
// Date        : 2019-10-28
// Version     : 1.0
// Parameter   : None
// Description :
//   Calculate branch & jump enable signal. It only evaluate the 'equal' and 'not equal'. All 'less than' is 
//   evaluated by other circuits and passed in by 'iLessThan'.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-28 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

`ifndef Disable_ZionRiscvIsaLib_BjEnNoLessThan
`ifdef ZionRiscvIsaLib_BjEnNoLessThan
  `__DefErr__(ZionRiscvIsaLib_BjEnNoLessThan)
`else
  `define ZionRiscvIsaLib_BjEnNoLessThan(UnitName,iBjExIf_MT,iLessThan_MT,oBjEn_MT) \
  ZionRiscvIsaLib_BjEnNoLessThan  UnitName(                                         \
                              .iBjExIf(iBjExIf_MT),                                 \
                              .iLessThan(iLessThan)                                 \
                              .oBjEn(oBjEn_MT)                                      \
                            )
`endif
module ZionRiscvIsaLib_BjEnNoLessThan
(
  ZionRiscvIsaLib_BjExItf.Ex iBjExIf,
  input                      iLessThan
  output logic [1:0]         oBjEn
);

  logic equal;
  always_comb begin
    equal    = (iBjExIf.s1 == iBjExIf.s2);
    oBjEn[1] =  iBjExIf.jump                   // jump instuction lead to branch&jump
              |(iBjExIf.beq & iBjExIf.equal)   // Beq  instuction lead to branch&jump
              |(iBjExIf.bne & !iBjExIf.equal); // Bne  instuction lead to branch&jump
    oBjEn[0] = (iBjExIf.blt & iLessThan)       // Blt  instuction lead to branch&jump
              |(iBjExIf.bge & !iLessThan);     // Bge  instuction lead to branch&jump
  end

  //TODO: add assert

endmodule: ZionRiscvIsaLib_BjEnNoLessThan
`endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module name : ZionRiscvIsaLib_BjEnGen
// Author      : Wenheng Ma
// Date        : 2019-10-28
// Version     : 1.0
// Parameter   : None
// Description :
//   Calculate the entire branch&jump enable without other input. If you need to design a high-performance core or
//   place branch&jump interface in a individual module without other instructions, please use this module.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-28 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

`ifndef Disable_ZionRiscvIsaLib_BjEnGen
`ifdef ZionRiscvIsaLib_BjEnGen
  `__DefErr__(ZionRiscvIsaLib_BjEnGen)
`else
  `define ZionRiscvIsaLib_BjEnGen(UnitName,iBjExIf_MT,oBjEn_MT) \
  ZionRiscvIsaLib_BjEnGen UnitName(                             \
                                .iBjExIf(iBjExIf_MT),           \
                                .oBjEn(oBjEn_MT)                \
                              )
`endif
module ZionRiscvIsaLib_BjEnGen
(
  ZionRiscvIsaLib_BjExItf.Ex iBjExIf,
  output logic [1:0]         oBjEn
);

  logic lessThan;
  always_comb begin
    lessThan = iBjExIf.LessThan();
  end
  ZionRiscvIsaLib_BjEnNoLessThan  U_ZionRiscvIsaLib_BjEnNoLessThan(
                                    .iBjExIf,
                                    .iLessThan(lessThan),
                                    .oBjEn
                                  );

endmodule: ZionRiscvIsaLib_BjEnGen
`endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module name : ZionRiscvIsaLib_JumpLinkPc
// Author      : Wenheng Ma
// Date        : 2019-10-28
// Version     : 1.0
// Parameter   : None
// Description :
//   Calculate Link PC.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-28 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

`ifndef Disable_ZionRiscvIsaLib_JumpLinkPc
`ifdef ZionRiscvIsaLib_JumpLinkPc
  `__DefErr__(ZionRiscvIsaLib_JumpLinkPc)
`else
  `define ZionRiscvIsaLib_JumpLinkPc(UnitName,iBjExIf_MT,oLinkPcIf_MT) \
ZionRiscvIsaLib_JumpLinkPc  UnitName(                                  \
                                .iBjExIf(iBjExIf_MT),                  \
                                .oLinkPcIf(oLinkPcIf_MT)               \
                              )
`endif
module ZionRiscvIsaLib_JumpLinkPc
(
  ZionRiscvIsaLib_BjExItf               iBjExIf,
  ZionRiscvIsaLib_BjExItf.JumpLinkPcOut oLinkPcIf
);

  always_comb begin
    oLinkPcIf.linkPc = iBjExIf.LinkPcGen();
  end

endmodule: ZionRiscvIsaLib_JumpLinkPc
`endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module name : ZionRiscvIsaLib_S1LinkOffsetMux
// Author      : Wenheng Ma
// Date        : 2019-10-28
// Version     : 1.0
// Parameter   : None
// Description :
//   Mux the normal s1 and link offset.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-28 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

`ifndef Disable_ZionRiscvIsaLib_S1LinkOffsetMux
`ifdef ZionRiscvIsaLib_S1LinkOffsetMux
  `__DefErr__(ZionRiscvIsaLib_S1LinkOffsetMux)
`else
  `define ZionRiscvIsaLib_S1LinkOffsetMux(UnitName,iBjExIf_MT,ooS1MuxIf_MT) \
ZionRiscvIsaLib_S1LinkOffsetMux  UnitName(                                  \
                                .iBjExIf(iBjExIf_MT),                       \
                                .ooS1MuxIf(ooS1MuxIf_MT)                    \
                              )
`endif
module ZionRiscvIsaLib_S1LinkOffsetMux
(
  ZionRiscvIsaLib_BjExItf          iBjExIf,
  ZionRiscvIsaLib_BjExItf.S1MuxOut oS1MuxIf
);

  always_comb begin
    oS1MuxIf.s1Fnl = iBjExIf.S1LinkOffsetMux();
  end

endmodule : ZionRiscvIsaLib_S1LinkOffsetMux
`endif

//endsection: BranchJumpEx +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

