//section: AddSubEx ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Circuits about Add and Sub instructions are provided in this section.
// These circuits also could be reused to calculate memory address or do less than operation.
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interface name : ZionRiscvIsaLib_AddSubExItf
// Author         : Wenheng Ma
// Date           : 2019-10-24
// Version        : 1.0
// Parameter      :
//   RV64 - indicate whether the circuit is for RV64 or not. 1:RV64.  2:RV32.
// Description    :
//   Define signals that ADD and SUB ISA nead.
//   It contains 6 instructions: ADD/ADDI, ADDW/ADDIW, SUB, SUBW.
//   Note that, for efficient architecture design, some other instruction may reuse the Adder. For example, memory
//   address could be calculated by the 'add' and 'less than' could be done by the 'sub'.
//   In the code, op indicate the operation type:
//     op[0] = add,     op[1] = sub,      op[2] = .W (only for R64I)
//   The interface could also be used to calculate the result of 'less than' compare by the LessThan function. 
//     - 'less than' is used in 8 instructions: BLT[U], BGE[U], BLT[I][U]
//   Parameter RV64 indicate whether the processor is 64-bit core with the ISA of RV64I.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-24 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
`ifndef Disable_ZionRiscvIsaLib_AddSubExItf
interface  ZionRiscvIsaLib_AddSubExItf
#(RV64 = 0);

  localparam CPU_WIDTH = 32*(RV64+1);
  logic [RV64     +1:0] op;
  logic [CPU_WIDTH-1:0] s1,s2,rslt;

  // TODO: add comments
  function automatic logic AddSubLessThan(input unsignedFlg, cmpRsltSign); 
    return ((unsignedFlg && (s1[$high(s1)] ^ s2[$high(s2)]))? s2[$high(s2)] : cmpRsltSign);
  endfunction : AddSubLessThan

  modport De (output op, s1, s2);
  modport Ex (input  op, s1, s2, output rslt);
  modport LessThan (input s1, s2, import AddSubLessThan);

endinterface : ZionRiscvIsaLib_AddSubExItf
`endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module name : ZionRiscvIsaLib_AddSubExec
// Author      : Wenheng Ma
// Date        : 2019-10-24
// Version     : 1.0
// Parameter   :
//   RV64 - indicate whether the circuit is for RV64 or not. 1:RV64.  2:RV32. It can be inferred from iAddSubExIf.
// Description :
//   Calculate addition or subtraction according to the op.
//   iAddSubExIf.op indicate the operation type.
//     op[0] = add,     op[1] = sub,      op[2] = .W (only for R64I)
//   Note that, add and sub can be activated at the same time. That will lead to an undefined result.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-24 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

`ifndef Disable_ZionRiscvIsaLib_AddSubExec
`ifdef ZionRiscvIsaLib_AddSubExec
  `__DefErr__(ZionRiscvIsaLib_AddSubExec)
`else
  `define ZionRiscvIsaLib_AddSubExec(UnitName,iAddSubExIf_MT)   \
`ifdef VIVADO_SYN                                               \
    localparam UnitName``_RV64 = iAddSubExIf_MT.RV64;           \
  `else                                                         \
    localparam UnitName``_RV64 = $bits(iAddSubExIf_MT.s1)/32-1; \
  `endif                                                        \
  ZionRiscvIsaLib_AddSubExec#(.RV64(UnitName``_RV64))           \
                            UnitName(                           \
                              .iAddSubExIf(iAddSubExIf_MT)      \
                            )
`endif
module ZionRiscvIsaLib_AddSubExec
#(RV64 = 0
)(
  ZionRiscvIsaLib_AddSubExItf.Ex iAddSubExIf
);

  localparam CPU_WIDTH = 32*(RV64+1);
  logic [CPU_WIDTH-1:0] s1, s1Tmp, s2, s2Tmp, rsltTmp;
  wire addEn = iAddSubExIf.op[0];
  wire subEn = iAddSubExIf.op[1];
  always_comb begin
    s1      = iAddSubExIf.s1;
    s2      = iAddSubExIf.s2;
    s1Tmp   = {$bits(s1){addEn|subEn}} & s1; //TODO: use BasicCircuitLib in the code
    s2Tmp   =   ({$bits(s2){subEn}} & ~s2)
              | ({$bits(s2){addEn}} &  s2);
    rsltTmp = (s1Tmp + s2Tmp + subEn);
  end
  `gen_if(RV64) begin : Rv64RsltGen
    wire WFlg = iAddSubExIf.op[2];
    assign iAddSubExIf.rslt = (WFlg) ? {{32{rsltTmp[31]}},rsltTmp[31:0]} : rsltTmp;
  end `gen_else begin : Rv32RsltGen
    assign iAddSubExIf.rslt = rsltTmp;
  end

  // Only one kind of operation can be done in a certain cycle. If both of addEn(iAddSubExIf.op[0])
  // and subEn(iAddSubExIf.op[1]) is 1, the result will be undifined and lead to an error. So it is
  // necessary to assert the situation.
 always_comb begin
    assert ($onehot0({addEn, subEn}))
    else $error("Signal Error: Both of addEn and subEn are activated which only one could work at a certain time.");
  end
endmodule : ZionRiscvIsaLib_AddSubExec
`endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module name : ZionRiscvIsaLib_AddSubLessThan
// Author      : Wenheng Ma
// Date        : 2019-10-24
// Version     : 1.0
// Parameter   : None
// Description :
//   Calculate less than flag according to the subtraction result(the highest bit). 
//   Less than operation could convert to a subtraction. So it can reuse the ZionRiscvIsaLib_AddSubExec module.
//   When use the subtractor, we need to deal with the unsigned less than operation which is done by this module.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-24 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

`ifndef Disable_ZionRiscvIsaLib_AddSubLessThan
`ifdef ZionRiscvIsaLib_AddSubLessThan
  `__DefErr__(ZionRiscvIsaLib_AddSubLessThan)
`else
  `define ZionRiscvIsaLib_AddSubLessThan(UnitName,iAddSubExIf_MT,iUnsignedFlg_MT,iCmpRsltSign_MT,oLessThan_MT) \
ZionRiscvIsaLib_AddSubLessThan  UnitName(                                                                      \
                                    .iAddSubExIf(iAddSubExIf_MT),                                              \
                                    .iUnsignedFlg(iUnsignedFlg_MT),                                            \
                                    .iCmpRsltSign(iCmpRsltSign_MT),                                            \
                                    .oLessThan(oLessThan_MT)                                                   \
                                  )
`endif
module ZionRiscvIsaLib_AddSubLessThan
(
  ZionRiscvIsaLib_AddSubExItf.LessThan iAddSubExIf,
  input iUnsignedFlg,
  input iCmpRsltSign,
  output logic oLessThan
);

  always_comb begin
    oLessThan = iAddSubExIf.AddSubLessThan(iUnsignedFlg,iCmpRsltSign);
  end

endmodule : ZionRiscvIsaLib_AddSubLessThan
`endif

//endsection: AddSubEx +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
