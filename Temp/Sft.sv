//section: ShiftEx +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Circuits about shift instructions are provided in this section.
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interface name : ZionRiscvIsaLib_SftExItf
// Author         : Wenheng Ma
// Date           : 2019-10-27
// Version        : 1.0
// Description    :
//   Define signals that shift(sft) operation ISA nead.
//   Shift operation contains 12 instructions: 
//     SLL  / SLLI ,        SRL  / SRLI ,        SRA  / SRAI ,  
//     SLLW / SLLIW,        SRLW / SRLIW,        SRAW / SRAIW, (instructions with .W is only used in R64I)
//   In the code, op indicate the operation type:
//     op[0] = shift left,            op[1] = shift right,     
//     op[2] = arithmetic shift,      op[3] = .W (only for R64I)
//   Parameter RV64 indicate whether the processor is 64-bit core with the ISA of RV64I.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-27 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
`ifndef Disable_ZionRiscvIsaLib_SftExItf
interface ZionRiscvIsaLib_SftExItf
#(RV64 = 0);

  localparam CPU_WIDTH = 32*(RV64+1);
  logic [2+RV64:0] op;
  logic [CPU_WIDTH-1:0] s1, rslt;
  logic [4+RV64:0]      s2;

  modport De (output op, s1, s2);
  modport Ex (input  op, s1, s2, output rslt);

endinterface: ZionRiscvIsaLib_SftExItf
`endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module name : ZionRiscvIsaLib_SftExItf
// Author      : Wenheng Ma
// Date        : 2019-10-27
// Version     : 1.0
// Parameter   :
//   RV64 - indicate whether the circuit is for RV64 or not. 1:RV64.  2:RV32. It can be inferred from iSftExIf.
// Description :
//   Shift execution.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-27 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

`ifndef Disable_ZionRiscvIsaLib_SftExec
`ifdef ZionRiscvIsaLib_SftExec
  `__DefErr__(ZionRiscvIsaLib_SftExec)
`else
  `define ZionRiscvIsaLib_SftExec(UnitName,iSftExIf_MT)      \
`ifdef VIVADO_SYN                                            \
    localparam UnitName``_RV64 = iSftExIf_MT.RV64;           \
  `else                                                      \
    localparam UnitName``_RV64 = $bits(iSftExIf_MT.s1)/32-1; \
  `endif                                                     \
  ZionRiscvIsaLib_SftExec#(.RV64(UnitName``_RV64))           \
                            UnitName(                        \
                              .iSftExIf(iSftExIf_MT)         \
                            )
`endif
module ZionRiscvIsaLib_SftExec
#(RV64 = 0
)(
  Disable_ZionRiscvIsaLib_SftExItf.Ex iSftExIf,
);
/*
  logic sftL, sftR, sftA, sftW;
  assign sftL = iSftExIf.op[0];
  assign sftR = iSftExIf.op[1];
  assign sftA = iSftExIf.op[2];
  `gen_if(RV64)begin
    assign sftW = iSftExIf.op[3];
  end

  type_Cpu s1W, s1Tmp, s1Reverse,sftRslt,rsltReverse,rsltReverseW;
  localparam DAT_WIDTH = $bits(type_Cpu);
  assign s1Reverse = {<<{iSftExIf.s1}}; 
  `gen_if(RV64) begin
    assign s1W = {((sftW)? {32{sftA & iSftExIf.s1[31]}} : iSftExIf.s1[63:32]) , iSftExIf.s1[31:0]};
  end `gen_else begin
    assign s1W = iSftExIf.s1;
  end
  assign s1Tmp = ({DAT_WIDTH{sftL}} & s1Reverse)
                |({DAT_WIDTH{sftR}} & s1W      );

  assign sftRslt = {{DAT_WIDTH{(sftA & s1Tmp[DAT_WIDTH-1])}},s1Tmp} >> s2;
  assign rsltReverse  = {<<{sftRslt}};
  `gen_if(RV64) begin
    assign rsltReverseW = signed'(rsltReverse[31:0]);
  end `gen_else begin
    assign rsltReverseW = rsltReverse[31:0];
  end
  
  assign oRslt = (sftL)? rsltReverseW : sftRslt;
*/

  localparam CPU_WIDTH = 32*(RV64+1);
  wire sftL = iSftExIf.op[0];
  wire sftR = iSftExIf.op[1];
  wire sftA = iSftExIf.op[2];
  wire  [4+RV64     :0] sftBits = iSftExIf.s2;
  logic [CPU_WIDTH-1:0] sftDat,rsltTmp;
  `gen_if(RV64)begin
    wire sftW = iSftExIf.op[3];
    assign sftDat = {((sftW)? iSftExIf.s1[31:0] : iSftExIf.s1[63:32]) , iSftExIf.s1[31:0]};
    wire [CPU_WIDTH-1:0] sftWRslt = signed'((sftR?rsltTmp[63:32]:'0) | (sftL?rsltTmp[31:0]:'0));//TODO: change to mask.
    assign iSftExIf.rslt = (sftW)? sftWRslt : rsltTmp;
  end `gen_else begin
    assign sftDat = iSftExIf.s1;
    assign iSftExIf.rslt = rsltTmp;
  end

  MultiTypeShift U_MultiTypeShift(sftR,sftA,sftL,sftDat,sftBits,rsltTmp);

  // Assertions for signal check.
  always_comb begin
    // There is no ShiftLeftArithmetic instruction, so sftA and sftL can not be activated simultaneously.
    assert ($onehot0({sftA, sftL})) ; 
    else $error("Signal Error: Both of sftA and sftL are activated which only one could work at a certain time.");
  end
  if(RV64=1) begin
    always_comb begin
      // For ShiftW instructions(SLLW/SRLW/SRAW), the highest bit in SftExIf.s2 is not used. 
      // Thus this bit must be 0 for ShiftW.
      assert ($onehot0({sftW, iSftExIf.s2[5]})) ; 
      else $error("Signal Error: Both of sftA and sftL are activated which only one could work at a certain time.");
    end
  end

module MultiTypeShift
#(INPUT_DATA_WIDTH  = 32,
  SHIFT_BIT_WIDTH   = 5 ,
  OUTPUT_DATA_WIDTH = 32 
)(
  input                                iSftR  ,
  input                                iSftA  ,
  input                                iSftL  ,
  input        [INPUT_DATA_WIDTH -1:0] iDat   ,
  input        [SHIFT_BIT_WIDTH  -1:0] iSftBit,
  output logic [OUTPUT_DATA_WIDTH-1:0] oDat
);

  wire [INPUT_DATA_WIDTH-1:0] datReverse, sftDat, highBits, sftRsltTmp, rsltReverse;
  always_comb begin
    datReverse  = {<<{iDat}};
    sftDat      = (iSftR?iDat:'0) | (iSftL?datReverse:'0); //TODO: change to mask.
    highBits    = {INPUT_DATA_WIDTH{iSftA&sftDat[$high(sftDat)]}}; //TODO: use library
    sftRsltTmp  = INPUT_DATA_WIDTH'({highBits,sftDat} >> iSftBit);
    rsltReverse = {<<{sftRsltTmp}};
    oDat        = (iSftR?sftRsltTmp:'0) | (iSftL?rsltReverse:'0); //TODO: change to mask.
  end

endmodule : MultiTypeShift

endmodule: ZionRiscvIsaLib_SftExec
`endif

//endsection: ShiftEx ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

