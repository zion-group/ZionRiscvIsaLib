
//section: IntEx +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Circuits integrate all execution module of RV32I/RV64I instructions.
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interface name : ZionRiscvIsaLib_IntInsExItf
// Author         : Wenheng Ma
// Date           : 2019-10-30
// Version        : 1.0
// Description    :
//   To simplify the Execution module design, the library has a parameterized integer exicution module. This interface
//   is used by the module to put all necessary signals together. 
//   The interface has 3 types of modport:
//     1. Modport for integer instructions excution with branch&jump and memory address calculate.
//        In this kind of modport, the Int Excution module takes charge of:
//          a) int instructions execution,  b) branch&jump instrucions execution,  c) memory address calculate
//     2. Modport for integer instructions excution with branch&jump
//        In this kind of modport, the Int Excution module takes charge of:
//          a) int instructions execution,  b) branch&jump instrucions execution
//     3. Modport for integer instructions excution without other actions
//        In this kind of modport, the Int Excution module only takes charge of int instructions execution
//   Parameter RV64 indicate whether the processor is 64-bit core with the ISA of RV64I.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-30 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
`ifndef Disable_ZionRiscvIsaLib_IntInsExItf
interface ZionRiscvIsaLib_IntInsExItf
#(RV64 = 0);

  localparam CPU_WIDTH = 32*(RV64+1);
  logic [CPU_WIDTH-1:0] pc, s1, s2, offset;
  logic [RV64:0] flags; // flags[0] - unsigned flag       flags[1] - .W flag
  logic addSubIns, addEn, subEn;
  logic andEn, orEn, xorEn;
  logic sltEn, bjEn, branch, beq, bne, blt, bge, jump;
  logic sftLeft, sftRight, sftA;
  logic memEn;
  logic BjEn;
  logic [1:0] linkOffset;
  logic [CPU_WIDTH-1:0] intRslt, BjTgt, memAddr;

  // modport for integer instructions excution with branch&jump and memory address calculate.
  modport IntBjMemDeOut (output pc, s1, s2, offset, flags, addEn, subEn, andEn, orEn, xorEn,
                                sftLeft, sftRight, sftA, sltEn,
                                addSubIns, bjEn, branch, beq, bne, blt, bge, jump, linkOffset, 
                                memEn
                        );
  modport IntBjMemExIn  (input  pc, s1, s2, offset, flags, addEn, subEn, andEn, orEn, xorEn,
                                sftLeft, sftRight, sftA, sltEn,
                                addSubIns, bjEn, branch, beq, bne, blt, bge, jump, linkOffset,
                                memEn
                        );
  modport IntBjMemExOut(output  intRslt, BjEn, BjTgt, memAddr);

  // modport for integer instructions excution with branch&jump.
  modport IntBjDeOut(output pc, s1, s2, offset, flags, addEn, subEn, andEn, orEn, xorEn,
                            sftLeft, sftRight, sftA, sltEn,
                            addSubIns, bjEn, branch, beq, bne, blt, bge, jump, linkOffset
                    );
  modport IntBjExIn (input  pc, s1, s2, offset, flags, addEn, subEn, andEn, orEn, xorEn,
                            sftLeft, sftRight, sftA, sltEn,
                            addSubIns, bjEn, branch, beq, bne, blt, bge, jump ,linkOffset
                    );
  modport IntBjExOut(output  intRslt, BjEn, BjTgt);

  // modport for integer instructions excution.
  modport IntDeOut(output pc, s1, s2, offset, flags, addEn, subEn, andEn, orEn, xorEn,
                          sftLeft, sftRight, sftA, sltEn
                  );
  modport IntExIn (input  pc, s1, s2, offset, flags, addEn, subEn, andEn, orEn, xorEn,
                          sftLeft, sftRight, sftA, sltEn
                  );
  modport IntExOut(output  intRslt);

endinterface: ZionRiscvIsaLib_IntInsExItf
`endif

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Module name : ZionRiscvIsaLib_IntEx
// Author      : Wenheng Ma
// Date        : 2019-08-14
// Version     : 1.0
// Description :
//   Integer instructions execution module. The module has 3 types(indicated by INT_MODULE_TYPE):
//     1. Int execution with branch&jump and memory address calculate. (INT_MODULE_TYPE == 0)
//        In this type, the module reuse the adder for memory address calculation and Link PC. Besides it reuse the 
//        sub for 'less than'(also for branch).
//     2. Int execution with branch&jump. (INT_MODULE_TYPE == 1)
//        In this type, the module reuse the adder for Link PC, and reuse the sub for 'less than'(also for branch).
//     3. Int execution. (INT_MODULE_TYPE == 2)
//        In this type, the module do not reuse anything. It has the best performance and the largest area.
//   When instantiated the module, the interface connection must be indicated according to the INT_MODULE_TYPE. Because
//   different module types has different signals and circuits.
//   Parameter RV64 indicate whether the processor is 64-bit core with the ISA of RV64I.
// Modification History:
//    Date    |   Author   |   Version   |   Change Description
//======================================================================================================================
// 2019-10-30 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

module ZionRiscvIsaLib_IntEx
#(RV64 = 0,
  INT_MODULE_TYPE = 0
)(
  ZionRiscvIsaLib_IntInsExItf iDat,
  ZionRiscvIsaLib_IntInsExItf oDat
);
`Use_ZionBasicCircuitLib(Bc)

  localparam CPU_WIDTH = 32*(RV64+1);
  logic sltRslt;
  wire unsignedFlg = iDat.flags[0];
  logic                 SltRslt;
  logic [CPU_WIDTH-1:0] BitsRslt;
  logic [CPU_WIDTH-1:0] SftRslt;
  logic [CPU_WIDTH-1:0] addSubRslt;
  logic [CPU_WIDTH-1:0] S1LinkOffsetMuxRlst;
  logic [CPU_WIDTH-1:0] BjTgtAddrRlst;
  ZionRiscvIsaLib_BitsExItf#(RV64) BitsIf();
  ZionRiscvIsaLib_SftExItf#(RV64) SftIf();
  ZionRiscvIsaLib_AddSubExItf#(RV64) AddSubIf();

  always_comb begin
    BitsIf.andEn   = iDat.andEn   ;
    BitsIf.orEn    = iDat.orEn    ;
    BitsIf.xorEn   = iDat.xorEn   ;
    BitsIf.s1      = iDat.s1      ;
    BitsIf.s2      = iDat.s2      ;

    SftIf.op[0]   = iDat.sftLeft  ;
    SftIf.op[1]   = iDat.sftRight ;
    SftIf.op[2]   = iDat.sftA     ;
    SftIf.s1      = iDat.s1       ;
    SftIf.s2[4:0] = iDat.s2[4:0]  ;

    AddSubIf.op[0] = iDat.addEn   ;
    AddSubIf.op[1] = iDat.subEn   ;
    AddSubIf.s2    = iDat.s2      ;
  end

  `gen_if(RV64) begin
    always_comb begin
      SftIf.op[3] =  iDat.flags[1];
      AddSubIf.op[2] = iDat.flags[1];
    end
  end

  `ZionRiscvIsaLib_BitsOpExec (U_BitsOpExec, BitsIf.Ex , BitsRslt);
  `ZionRiscvIsaLib_SftExec    (U_SftExec   , SftIf , SftRslt);
  `ZionRiscvIsaLib_AddSubExec (U_AddSubExec, AddSubIf , addSubRslt);

  `gen_if(INT_MODULE_TYPE==0 | INT_MODULE_TYPE==1) begin: IntModuleType_0_1
    // wire [CPU_WIDTH-1:0] addSubRslt = AddSubIf.rslt;
    // wire lessThanFlg = addSubRslt[$high(addSubRslt)]; //TODO: use HighB
    wire lessThanFlg = `BcHighB(addSubRslt);
    `ZionRiscvIsaLib_S1LinkOffsetMux (U_S1LinkOffsetMux, BjIf.Ex, S1LinkOffsetMuxRlst);
    ZionRiscvIsaLib_BjExItf#(RV64) BjIf();
    always_comb begin
      AddSubIf.s1    = S1LinkOffsetMuxRlst;

      BjIf.bjEn        = iDat.bjEn;
      BjIf.branch      = iDat.branch;
      BjIf.beq         = iDat.beq;
      BjIf.bne         = iDat.bne;
      BjIf.blt         = iDat.blt;
      BjIf.bge         = iDat.bge;
      BjIf.unsignedFlg = unsignedFlg;
      BjIf.jump        = iDat.jump;
      BjIf.linkOffset  = iDat.linkOffset;
      BjIf.pc          = iDat.pc;
      BjIf.s1          = iDat.s1;
      BjIf.s2          = iDat.s2;
      BjIf.offset      = iDat.offset;

      // oDat.intRslt = BitsRslt  | ({$bits(addSubRslt){iDat.addSubIns}} & addSubRslt)  //TODO: use mask
      //               |SftRslt   | {{(CPU_WIDTH-1){1'b0}},{sltRslt & iDat.sltEn}};//TODO: use ZeroExtd
      oDat.intRslt = BitsRslt  | `BcMaskM(iDat.addSubIns,addSubRslt) | `BcMaskM(iDat.jump,addSubRslt)
                    |SftRslt   | `BcZeroExtdM((sltRslt & iDat.sltEn),CPU_WIDTH);
      oDat.BjTgt   = BjTgtAddrRlst;
    end
    `ZionRiscvIsaLib_AddSubLessThan (U_AddSubLessThan, AddSubIf, unsignedFlg, lessThanFlg, sltRslt);
    `ZionRiscvIsaLib_BjEnNoLessThan (U_BjEnNoLt , BjIf.Ex, lessThanFlg, oDat.BjEn );
    `ZionRiscvIsaLib_BjTgtAddr(U_BjTgtAddr, BjIf.Ex,              BjTgtAddrRlst);
    `gen_if(INT_MODULE_TYPE==0)  
        assign oDat.memAddr = (iDat.memEn)? addSubRslt : '0;
  end
  `gen_elif(INT_MODULE_TYPE==2) begin: IntModuleType_2
    ZionRiscvIsaLib_SltExItf#(RV64) SltIf();
    always_comb begin
      AddSubIf.s1    = iDat.s1;
      SltIf.en          = iDat.sltEn;
      SltIf.unsignedFlg = unsignedFlg;
      SltIf.s1 = iDat.s1;
      SltIf.s2 = iDat.s2;
      oDat.intRslt = BitsRslt | SftRslt | addSubRslt | {{(CPU_WIDTH-1){1'b0}},{SltRslt}};
    end
    `ZionRiscvIsaLib_SetLessThan(U_SetLessThan,SltIf,SltRslt);
  end
`Unuse_ZionBasicCircuitLib(Bc)
endmodule: ZionRiscvIsaLib_IntEx

//endsection: IntEx ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

