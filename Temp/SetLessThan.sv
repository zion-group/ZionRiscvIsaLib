
//section: SetLessThanEx +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Circuits about Set Less Than instructions are provided in this section.
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interface name : ZionRiscvIsaLib_SltExItf
// Author         : Wenheng Ma
// Date           : 2019-10-27
// Version        : 1.0
// Parameter      :
//   RV64 - indicate whether the circuit is for RV64 or not. 1:RV64.  2:RV32. It can be inferred from iSftExIf.
// Description    :
//   Define signals that SLT(set less than) operation ISA nead. And offer an Excution function to get the operation 
//   result. Note that this interface can be reused by the branch instructions.
//   'Set less than' contains 4 instructions: SLT/SLTI, SLTU/SLTIU.
//   'Branch' contains 4 instructions: BLT/BLTU, BGE/BGEU.
//   Parameter RV64 indicate whether the processor is 64-bit core with the ISA of RV64I.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-27 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
`ifndef Disable_ZionRiscvIsaLib_SltExItf
interface ZionRiscvIsaLib_SltExItf
#(RV64 = 0);

  localparam CPU_WIDTH = 32*(RV64+1);
  logic en, unsignedFlg, rslt;
  logic [CPU_WIDTH-1:0] s1,s2;

  // Extend data according to unsigned flag(unsignedFlg), then compare the size of s1 and s2.
  // With the extension of data, we can reuse the compare circuits for both signed and unsigned compare operations.
  function automatic logic Exec;

    logic signed [CPU_WIDTH:0] s1Extd, s1Mask, s2Extd, s2Mask;
    s1Extd = {((~unsignedFlg) & s1[CPU_WIDTH-1]) , s1}; // Extend s1 according unsignedFld // TODO: use HighBit
    s2Extd = {((~unsignedFlg) & s2[CPU_WIDTH-1]) , s2}; // Extend s2 according unsignedFld// TODO: use HighBit
    s1Mask = {$bits(s1Extd){en}} & s1Extd; // TODO: use Mask
    s2Mask = {$bits(s2Extd){en}} & s2Extd; // TODO: use Mask
    return ((s1Mask<s2Mask)? 1'b1:1'b0);

  endfunction: Exec

  modport De (output en, unsignedFlg, s1, s2);
  modport Ex (input  en, unsignedFlg, s1, s2, output rslt, import Exec);

endinterface: ZionRiscvIsaLib_SltExItf
`endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module name : ZionRiscvIsaLib_SetLessThan
// Author      : Wenheng Ma
// Date        : 2019-10-27
// Version     : 1.0
// Parameter   : None
// Description :
//   Less than execution.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-27 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

`ifndef Disable_ZionRiscvIsaLib_SetLessThan
`ifdef ZionRiscvIsaLib_SetLessThan
  `__DefErr__(ZionRiscvIsaLib_SetLessThan)
`else
  `define ZionRiscvIsaLib_SetLessThan(UnitName,iSltExIf_MT,oLessThan_MT) \
  ZionRiscvIsaLib_SetLessThan UnitName(                                  \
                                .iSltExIf(iSltExIf_MT)                   \
                              )
`endif
module ZionRiscvIsaLib_SetLessThan
(
  ZionRiscvIsaLib_SltExItf.Ex iSltExIf
);

  always_comb begin
    iSltExIf.rslt = iSltExIf.Exec();
  end

endmodule: ZionRiscvIsaLib_SetLessThan
`endif

//endsection: SetLessThanEx ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++