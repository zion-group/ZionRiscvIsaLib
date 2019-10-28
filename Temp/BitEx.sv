//section: BitOperationEx ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Circuits about bit operation instructions are provided in this section.
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interface name : ZionRiscvIsaLib_BitsExItf
// Author         : Wenheng Ma
// Date           : 2019-08-02
// Version        : 1.0
// Description    :
//   Define signals that bit operation ISA nead. And offer an Excution function to get the bit operation result.
//   Bit operation contains 6 instructions: AND/ANDI, OR/ORI, XOR/XORI.
//   Note that, for efficient architecture design, some other instructions could convert to bit operation. For example,
//   LUI only place a U-type immediate into the highest bit of regfile. Thus the fixed shift could be done in Decode,
//   and do an 'or' operation with 0.
//   Parameter RV64 indicate whether the processor is 64-bit core with the ISA of RV64I.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-24 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
`ifndef Disable_ZionRiscvIsaLib_BitsExItf
interface ZionRiscvIsaLib_BitsExItf
#(RV64 = 0);

  localparam CPU_WIDTH = 32*(RV64+1);
  logic andEn, orEn, xorEn;
  logic [CPU_WIDTH-1:0] s1, s2, rslt;

  // Get bit operation result. If more than 1 'xxEn' signals are acctivated, the result will be 
  // undifined and lead to an error.
  function automatic logic [CPU_WIDTH-1:0] Exec();
    logic [CPU_WIDTH-1:0] rslt;
    unique case (1'b1)
      andEn  : rslt = (s1 & s2); // and calculation
      orEn   : rslt = (s1 | s2); // or  calculation
      xorEn  : rslt = (s1 ^ s2); // xor calculation
      default: rslt = '0;
    endcase
    return rslt;
  endfunction

  modport De (output andEn, orEn, xorEn, s1, s2);
  modport Ex (input  andEn, orEn, xorEn, s1, s2, output rslt, import Exec);

endinterface: ZionRiscvIsaLib_BitsExItf
`endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module name : ZionRiscvIsaLib_BitsOpExec
// Author      : Wenheng Ma
// Date        : 2019-10-24
// Version     : 1.0
// Parameter   : None
// Description :
//   Calculate all bit operations. Only one signal in iBitsExif.andEn, iBitsExif.orEn and iBitsExif.xorEn can be 1.
//   If two or three of these 'En' signals are 1, the result are undefined.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-24 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

`ifndef Disable_ZionRiscvIsaLib_BitsOpExec
`ifdef ZionRiscvIsaLib_BitsOpExec
  `__DefErr__(ZionRiscvIsaLib_BitsOpExec)
`else
  `define ZionRiscvIsaLib_BitsOpExec(UnitName,iBitsExif_MT) 
ZionRiscvIsaLib_BitsOpExec UnitName(.iBitsExif(iBitsExif_MT));
`endif
module ZionRiscvIsaLib_BitsOpExec
(
  ZionRiscvIsaLib_BitsExItf.Ex iBitsExif
);

  always_comb begin
    iBitsExif.rslt   = iBitsExif.Exec();
  end

  // Only one kind of operation can be done in a certain cycle. If more than 1 'xxEn' signals are acctivated,
  // the result will be undifined and lead to an error. So it is necessary to assert the situation.
  always_comb begin
    assert($onehot0{iBitsExif.andEn, iBitsExif.orEn, iBitsExif.xorEn}) ;
    else $error("Signal Error: More than 1 'xxEn' signals are activated in iBitsExif.andEn, iBitsExif.orEn and iBitsExif.xorEn which only one could work.");
  end

endmodule: ZionRiscvIsaLib_BitsOpExec
`endif

//section: BitOperationEx ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

