///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interface name : ZionRiscvIsaLib_RvimazDecodeItf
// Author         : Wenheng Ma
// Date           : 2019-08-10
// Version        : 1.0
// Description    :
//   The interface is used for riscv general decode, including: 
//     - RV32I, RV64I, 'M' Extension, 'A' Extension, 'Ziscr'
//   Float instructions and RV32C is not included. All reusable signals and logic circuits are defined in the 
//   interface. Users call the functions to get a enable or flag signals. The interface can not be used at port. Thus,
//   it has no modport.
//   Parameter RV64 indicate whether the processor is 64-bit core with the ISA of RV64I.
// Modification History:
//   Date   |   Author   |   Version   |   Change Description
//======================================================================================================================
// 19-08-10 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
`ifndef Disable_ZionRiscvIsaLib_RvimazDecodeItf
interface ZionRiscvIsaLib_RvimazDecodeItf
#(RV64  = 0
)(
  input [31:0] ins
);

  localparam CPU_WIDTH = 32*(RV64+1);
  typedef logic signed [CPU_WIDTH-1:0] type_Signed;
  logic [6:0] funct7;
  logic [2:0] funct3;
  logic [4:0] opHigh;
  logic [1:0] opLow ;
  logic [4:0] rs1, rs2, rd;
  logic [CPU_WIDTH-1:0] shamt, immItype, immStype, immBtype, immUtype, immJtype;
  always_comb begin
    {funct7,rs2,rs1,funct3,rd,opHigh,opLow} =ins;
    immItype = unsigned'(type_Signed'({ins[31:20]}));
    immStype = unsigned'(type_Signed'({ins[31:25],ins[11:7]}));
    immBtype = unsigned'(type_Signed'({ins[31],ins[7],ins[30:25],ins[11:8],1'b0}));
    immUtype = unsigned'(type_Signed'({ins[31:12],12'h000}));
    immJtype = unsigned'(type_Signed'({ins[31],ins[19:12],ins[20],ins[30:21],1'b0}));
    shamt    = unsigned'(type_Signed'({ins[(24+RV64):20]}));
    uimm     = unsigned'(type_Signed'({ins[19:15]}));
  end

  logic [7:0] f3Oh;
  logic f7h6_000000, f7h6_010000, f7_0000000, f7_0000001, f7_0100000;
  always_comb begin
    foreach(f3Oh) f3Oh[i] = (funct3 == i);
    f7h6_000000 = (funct7[6:1] == 6'b000000);
    f7h6_010000 = (funct7[6:1] == 6'b010000);
    f7_0000000  = f7h6_000000 & ~funct7[0];
    f7_0000001  = f7h6_000000 &  funct7[0];
    f7_0100000  = f7h6_010000 & ~funct7[0];
  end

  logic notRvc, bigImmInsFlg, opHigh_110xx, bjInsFlg, jumpInsFlg, branchInsFlg;
  logic loadStoreInsFlg, loadInsFlg, storeInsFlg;
  logic intInsFlg, intImmInsFlg, intRs2InsFlg, wInsFLg;
  logic sysInsFlg, atomicInsFlg, fenceInsFlg;
  always_comb begin
    notRvc          = (opLow==2'b11);                  // whether it's RVC instructions. 0-RVC ins.  1-Not RVC ins.
    bigImmInsFlg    = (opHigh ==? 5'b0?101) & notRvc;  // big immediate operand instructions: LUI, AUIPC
    opHigh_110xx    = (opHigh ==  5'b110??);           // the signal is used in branch and jump decode.
    bjInsFlg        = opHigh_110xx & (opHigh[1:0]!=2'b10) & notRvc; // branch and jump instructions
    jumpInsFlg      = opHigh_110xx & opHigh[0] & notRvc;            // jump instructions: JAL, JALR
    branchInsFlg    = opHigh_110xx & (opHigh[1:0]==2'b00) & notRvc; // branch instructions: BEQ, BNE, BLE[U], BGE[U]
    loadStoreInsFlg = (opHigh ==? 5'b0?000) & notRvc;  // load & store : 00000 - int load , 01000 - int store
    loadInsFlg      = loadStoreInsFlg & ~opHigh[3];    // load instructions:  LB[U], LH[U], LW[U], LD
    storeInsFlg     = loadStoreInsFlg &  opHigh[3];    // store instructions: SB[U], SH[U], SW[U], SD
    intInsFlg       = (opHigh ==? 5'b0?1?0) & notRvc;  // all integer computing operation including:
                                                       //   - ADD[I][W], SUB[W], OR[I], AND[I], XOR[I] 
                                                       //   - SLT[I][U], SLL[I][W], SRL[I][W], SRA[I][W]
                                                       //   - instructions in RV32M and RV64M
    intImmInsFlg    = intInsFlg & ~opHigh[3];          // integer computing instructions using immediate operand 
    intRs2InsFlg    = intInsFlg &  opHigh[3];          // integer computing instructions using rs2
                                                       //   - ADD[W],SUB[W],OR,AND,XOR,SLT[U],SLL[W],SRL[W],SRA[W]
                                                       //   - instructions in RV32M and RV64M
    wInsFlg         = intInsFlg & opHigh[1];           // indicate whether it's .W instructions. It's only used in RV64.
    sysInsFlg       = (opHigh ==  5'b11100) & notRvc;  // CSR instructions(RvZicsr): CSRRW[I], CSRRS[I], CSRC[I]
    atomicInsFlg    = (opHigh ==  5'b01011) & notRvc;  // atomic instructions(RV32A): 22 instructions in RV32A
    fenceInsFlg     = (opHigh ==  5'b00011) & notRvc & (funct3 ==? 3'b00?); // fence instructions: FENCE, FENCE.I
  end 

  logic jalFlg, jalrFlg, fenceIFlg, csrUimmFlg;
  always_comb begin
    jalFlg     = jumpInsFlg  &  opHigh[1];           // JAL 
    jalrFlg    = jumpInsFlg  & ~opHigh[1] & f3Oh[0]; // JALR 
    fenceIFlg  = fenceInsFlg &  f3Oh[0];             // FENCE.I
    csrUimmFlg = sysInsFlg   &  funct3[2];           // csr instructions with uimm: CSRRWI, CSRRSI, CSRRCI
  end

  logic rs1Enable, rs2Enable, rdEnable, insRtype, insItype, insStype, insBtype, insUtype, insJtype;
  always_comb begin
    insRtype = intRs2InsFlg | atomicInsFlg; // Rtype is used in int instructions with rs2 and atomic instructions.
    // Itype is used in: load instructions, int instructions with immediate operand, JALR and FENCE.I
    insItype  = loadInsFlg | intImmInsFlg | jalrFlg | fenceIFlg;   
    insStype  = storeInsFlg ; // Stype is only used for store instructions
    insBtype  = branchInsFlg; // Btype is only used for branch instructions
    insUtype  = bigImmInsFlg; // Utype is only used for big immediate instructions
    insJtype  = jalFlg      ; // Jtype is only used for JAL
    rdEnable  = ~(insStype & insBtype);              // only Stype and Btype instructions do not have rd.
    rs1Enable = ~(insUtype & insJtype & csrUimmFlg); // only Utype, Jtype and csr with imm do not have rs1.
    rs2Enable = insItype | insStype | insBtype;      // only Itype, Stype and Btype have rs2.
  end

  logic intNoWImmInsFlg, intWImmInsFlg, intNoWRs2InsFlg, intWRs2InsFlg;
  logic simpleIntRs2InsFlg, simpleIntNoWRs2InsFlg, simpleIntWRs2InsFlg;
  always_comb begin
    intNoWImmInsFlg        = intImmInsFlg    & ~opHigh[1];
    intWImmInsFlg          = intImmInsFlg    &  opHigh[1];
    intNoWRs2InsFlg        = intRs2InsFlg    & ~opHigh[1];
    intWRs2InsFlg          = intRs2InsFlg    &  opHigh[1];
    simpleIntRs2InsFlg     = intRs2InsFlg    & f7_0000000;
    simpleIntNoWRs2InsFlg  = intNoWRs2InsFlg & f7_0000000;
    simpleIntWRs2InsFlg    = intWRs2InsFlg   & f7_0000000;
  end

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Big immediate operand instructions: LUI, AUIPC 
  function automatic logic BigImmIns();
    return (bigImmInsFlg);
  endfunction: bigImmIns

  function automatic logic Lui(); // LUI valid
    return (bigImmInsFlg & opHigh[3]);
  endfunction: Lui

  function automatic logic Auipc(); // AUIPC valid
    return (bigImmInsFlg & ~opHigh[3]);
  endfunction: Auipc
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Branch and jump instructions: JAL, JALR, BEQ, BNE, BLT[U], BGE[U]
  function automatic logic BranchJumpIns(); // Branch or jump instructions valid
    return (bjIns);
  endfunction: BranchJumpIns

  // Jump instructions--------------------------------------------------------------------------------------------------
  function automatic logic JumpIns(); // Jump instructions: JAL, JALR
    return (jumpInsFlg);
  endfunction: JumpIns

  function automatic logic Jal(); // JAL valid
    return (jalFlg);
  endfunction: Jal

  function automatic logic Jalr(); // JALR valid
    return (jalrFlg);
  endfunction: Jalr

  // Branch instructions------------------------------------------------------------------------------------------------
  function automatic logic BranchIns(); // Branch instructions: BEQ, BNE, BLT[U], BGE[U]
    return (branchInsFlg);
  endfunction: BranchIns

  function automatic logic BranchUnsigned(); // Branch with unsigned compare: BLTU, BGEU
    return (branchInsFlg & funct3[1]);
  endfunction: BranchUnsigned

  function automatic logic BranchEqNe(); // Branch with equal estimation: BEQ, BNE
    return (branchInsFlg & ~funct3[2]);
  endfunction: BranchEqNe

  function automatic logic BranchSizeCmp(); // Branch with size compare: BLT[U], BGE[U]
    return (branchInsFlg & funct3[2]);
  endfunction: BranchSizeCmp

  function automatic logic BranchRevers(); // Reverse the equal or compare result: BEQ<->BNE,   BLT<->BGE
    return (branchInsFlg & funct3[0]);     // If you use a circuit to estimate rs1==rs2 to execute BEQ, BNE will set
  endfunction: BranchRevers                // 'BranchRevers' to 1, then you may reverse the equal result to run BNE.

  function automatic logic Beq(); // Beq valid
    return (branchInsFlg & f3Oh[0]);
  endfunction: Beq

  function automatic logic Bne(); // Bne valid
    return (branchInsFlg & f3Oh[1]);
  endfunction: Bne

  function automatic logic Blt(); // Blt valid
    return (branchInsFlg & f3Oh[4]);
  endfunction: Blt

  function automatic logic Bge(); // Bge valid
    return (branchInsFlg & f3Oh[5]);
  endfunction: Bge

  function automatic logic Bltu(); // Bltu valid
    return (branchInsFlg & f3Oh[6]);
  endfunction: Bltu

  function automatic logic Bgeu(); // Bgeu valid
    return (branchInsFlg & f3Oh[7]);
  endfunction: Bgeu
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Load and store instructions: LB[U], LH[U], LW[U], SB, SH, SW
  function automatic logic LoadStoreIns(); // Load or Store instructions valid
    return (loadStoreInsFlg);
  endfunction: LoadStoreIns

  function automatic logic[1:0] LoadStoreWidth(); // Operation width of load of store
    return ({2{loadStoreInsFlg}} & funct3[1:0]);  // 00-Byte     01-Half word     10-Word     11-Double word
  endfunction:

  // Load instructions--------------------------------------------------------------------------------------------------
  function automatic logic LoadIns(); // Load instructions valid
    return (loadInsFlg);
  endfunction: LoadIns

  function automatic logic LoadUnsigned(); // Load with unsigned extend
    return (loadInsFlg & funct3[2]);
  endfunction: LoadUnsigned

  function automatic logic Lb(); // LB valid
    return (loadInsFlg & f3Oh[0]);
  endfunction: Lb

  function automatic logic Lh(); // LH valid
    return (loadInsFlg & f3Oh[1]);
  endfunction: Lh 

  function automatic logic Lw(); // LW valid
    return (loadInsFlg & f3Oh[2]);
  endfunction: Lw 

  function automatic logic Ld(); // LD valid
    return (loadInsFlg & f3Oh[3]);
  endfunction: Ld

  function automatic logic Lbu(); // LBU valid
    return (loadInsFlg & f3Oh[4]);
  endfunction: Lbu

  function automatic logic Lhu(); // LHU valid
    return (loadInsFlg & f3Oh[5]);
  endfunction: Lhu

  function automatic logic Lwu(); // LWU valid
    return (loadInsFlg & f3Oh[6]);
  endfunction: Lwu

  // Store instructions-------------------------------------------------------------------------------------------------
  function automatic logic StoreIns(); // Store Instructions valid
    return (storeInsFlg);
  endfunction: StoreIns

  function automatic logic Sb(); // SB valid
    return (storeInsFlg & f3Oh[0]);
  endfunction: Sb

  function automatic logic Sh(); // SH valid
    return (storeInsFlg & f3Oh[1]);
  endfunction: Sh

  function automatic logic Sw(); // SW valid
    return (storeInsFlg & f3Oh[2]);
  endfunction: Sw

  function automatic logic Sd(); // SD valid
    return (storeInsFlg & f3Oh[3]);
  endfunction: Sd
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // all integer computing operation including:
  //   - ADD[I][W], SUB[W], OR[I], AND[I], XOR[I] 
  //   - SLT[I][U], SLL[I][W], SRL[I][W], SRA[I][W]
  //   - instructions in RV32M and RV64M
  function automatic logic IntIns();
    return (intInsFlg);
  endfunction: IntIns

  function automatic logic IntWIns();
    return (wInsFlg);
  endfunction: IntWIns

  // ADD and SUB instructions-------------------------------------------------------------------------------------------
  function automatic logic AddSubIns(); // ADD[i][W] or SUB[W] valid
    return (f3Oh[0] & ((intImmInsFlg) | (intRs2InsFlg & (f7_0000000 | f7_0100000))));
  endfunction: AddSubIns

  function automatic logic AddSubWIns(); // ADD[i]W or SUBW valid
    return (f3Oh[0] & ((intWImmInsFlg) | (intWRs2InsFlg & (f7_0000000 | f7_0100000))));
  endfunction: AddSubWIns

  function automatic logic AddIns(); // ADD[i] valid
    return (f3Oh[0] & (intImmInsFlg | simpleIntRs2InsFlg));
  endfunction: AddIns

  function automatic logic AddWIns(); // ADD[i]W valid
    return (f3Oh[0] & (intWImmInsFlg | simpleIntWRs2InsFlg));
  endfunction: AddWIns

  function automatic logic Add(); // ADD valid
    return (f3Oh[0] & simpleIntNoWRs2InsFlg);
  endfunction: Add

  function automatic logic Addw(); // ADDW valid
    return (f3Oh[0] & simpleIntWRs2InsFlg);
  endfunction: Addw

  function automatic logic Addi(); // ADDI valid
    return (f3Oh[0] & intNoWImmInsFlg);
  endfunction: Addi

  function automatic logic Addiw(); // ADDIW valid
    return (f3Oh[0] & intWImmInsFlg);
  endfunction: Addiw

  function automatic logic Sub(); // SUB valid
    return (f3Oh[0] & (intNoWRs2InsFlg & f7_0100000));
  endfunction: Sub

  function automatic logic Subw(); // SUBW valid
    return (f3Oh[0] & (intWRs2InsFlg & f7_0100000));
  endfunction: Subw

  // Bit operate instructions-------------------------------------------------------------------------------------------
  //   - AND[i], OR[i], XOR[i]
  function automatic logic BitOperateIns(); // Bit operate valid
    return ((f3Oh[4] & f3Oh[6] & f3Oh[7]) & (intNoWImmInsFlg | simpleIntNoWRs2InsFlg));
  endfunction: BitOperateIns

  function automatic logic XorIns(); // XOR[I] valid
    return (f3Oh[4] & (intNoWImmInsFlg | simpleIntNoWRs2InsFlg));
  endfunction: Xor

  function automatic logic Xor(); // XOR valid
    return (f3Oh[4] & simpleIntNoWRs2InsFlg);
  endfunction: Xor

  function automatic logic Xori(); // XORi valid
    return (f3Oh[4] & intNoWImmInsFlg);
  endfunction: Xori

  function automatic logic OrIns(); // OR[I] valid
    return (f3Oh[6] & (intNoWImmInsFlg | simpleIntNoWRs2InsFlg));
  endfunction: OrIns

  function automatic logic Or(); // OR valid
    return (f3Oh[6] & simpleIntNoWRs2InsFlg);
  endfunction: Or

  function automatic logic Ori(); // ORI valid
    return (f3Oh[6] & intNoWImmInsFlg);
  endfunction: Ori

  function automatic logic AndIns(); // AND[I] valid
    return (f3Oh[7] & (intNoWImmInsFlg | simpleIntNoWRs2InsFlg));
  endfunction: AndIns

  function automatic logic And(); // AND valid
    return (f3Oh[7] & simpleIntNoWRs2InsFlg);
  endfunction: And

  function automatic logic Andi(); // ANDI valid
    return (f3Oh[7] & intNoWImmInsFlg);
  endfunction: Andi

  // Shift instructions-------------------------------------------------------------------------------------------------
  // SLL[I][W], SRL[I][W], SRA[I][W]
  function automatic logic SftIns(); // Shift instructions valid
    return ((op[1:0]==2'b01) & ((intImmInsFlg) | (intRs2InsFlg & (f7_0000000 | f7_0100000))));
  endfunction: SftIns

  function automatic logic SftWIns(); // W type shift valid: SLL[I]W, SRL[I]W, SRA[I]W
    return ((op[1:0]==2'b01) & ((intWImmInsFlg) | (intWRs2InsFlg & (f7_0000000 | f7_0100000))));
  endfunction: SftWIns

  function automatic logic SllIns(); // Shift left instructions valid
    return (f3Oh[1] & (intImmInsFlg | simpleIntRs2InsFlg));
  endfunction: SllIns

  function automatic logic SllWIns(); // Shift left .W instructions valid
    return (f3Oh[1] & (intWImmInsFlg | simpleIntWRs2InsFlg));
  endfunction: SllWIns

  function automatic logic Sll(); // SLL valid
    return (f3Oh[1] & simpleIntNoWRs2InsFlg);
  endfunction: Sll

  function automatic logic Sllw(); // SLLW valid
    return (f3Oh[1] & simpleIntWRs2InsFlg);
  endfunction: Sllw

  function automatic logic Slli(); // SRLI valid
    logic result;
    `gen_if(RV64==1)begin
      result = (f3Oh[1] & intNoWImmInsFlg & f7h6_000000);
    end `gen_else begin
      result = (f3Oh[1] & intNoWImmInsFlg & f7_0000000);
    end
    return result;
  endfunction: Slli


  function automatic logic Slliw(); // SLLIW valid
    return (f3Oh[1] & intWImmInsFlg);
  endfunction: Slliw

  function automatic logic SrIns(); // Shift right instructions valid
    return (f3Oh[5] & ((intImmInsFlg) | (intRs2InsFlg & (f7_0000000 | f7_0100000))));
  endfunction: SrIns

  function automatic logic SrWIns(); // Shift right .W instructions valid
    return (f3Oh[5] & ((intWImmInsFlg) | (intWRs2InsFlg & (f7_0000000 | f7_0100000))));
  endfunction: SrWIns

  function automatic logic SraIns(); // Shift right arithmetic valid
    return (f3Oh[5] & ((intImmInsFlg) | (intRs2InsFlg & f7_0100000)));
  endfunction: SraIns

  function automatic logic SraWIns(); // Shift right .W arithmetic valid
    return (f3Oh[5] & ((intWImmInsFlg) | (intWRs2InsFlg & f7_0100000)));
  endfunction: SraWIns

  function automatic logic Srl(); // SRL valid
    return (f3Oh[5] & simpleIntNoWRs2InsFlg);
  endfunction: Srl

  function automatic logic Srlw(); // SRLW valid
    return (f3Oh[5] & simpleIntWRs2InsFlg);
  endfunction: Srlw

  function automatic logic Sra(); // SRA valid
    return (f3Oh[5] & (intNoWRs2InsFlg & f7_0100000));
  endfunction: Sra

  function automatic logic Sraw(); // SRAW valid
    return (f3Oh[5] & (intWRs2InsFlg & f7_0100000));
  endfunction: Sraw

  function automatic logic Srli(); // SRLI valid
    logic result;
    `gen_if(RV64==1)begin
      result = (f3Oh[5] & intNoWImmInsFlg & f7h6_000000);
    end `gen_else begin
      result = (f3Oh[5] & intNoWImmInsFlg & f7_0000000);
    end
    return result;
  endfunction: Srli

  function automatic logic Srliw(); // SRLIW valid
    return (f3Oh[5] & intWImmInsFlg & f7_0000000);
  endfunction: Srliw

  function automatic logic Srai(); // SRAI valid
    logic result;
    `gen_if(RV64==1)begin
      result = (f3Oh[5] & intNoWImmInsFlg & f7h6_010000);
    end `gen_else begin
      result = (f3Oh[5] & intNoWImmInsFlg & f7_0100000);
    end
    return result;
  endfunction: Srai

  function automatic logic Sraiw(); // SRAIW valid
    return (f3Oh[5] & intWImmInsFlg & f7_0100000);
  endfunction: Sraiw

  // Less than instructions---------------------------------------------------------------------------------------------
  function automatic logic SltIns(); // Set less than instructions: SLT[I][U]
    return ((funct3 ==? 3'b01?) & (intNoWImmInsFlg | simpleIntNoWRs2InsFlg));
  endfunction: SltIns

  function automatic logic SltUnsigned(); // Unsigned set less than instructions: SLT[I]U
    return (f3Oh[3] & (intNoWImmInsFlg | simpleIntNoWRs2InsFlg));
  endfunction: SltUnsigned

  function automatic logic Slt(); // SLT valid
    return (f3Oh[2] & simpleIntNoWRs2InsFlg);
  endfunction: Slt

  function automatic logic Sltu(); // SLTU valid
    return (f3Oh[3] & simpleIntNoWRs2InsFlg);
  endfunction: Sltu

  function automatic logic Slti(); // SLTI valid
    return (f3Oh[2] & intNoWImmInsFlg);
  endfunction: Slt

  function automatic logic Sltiu(); // SLTIU valid
    return (f3Oh[3] & intNoWImmInsFlg);
  endfunction: Sltiu

  // Mul & Div instructions---------------------------------------------------------------------------------------------
  // MUL[H][U/SU], DIV[U], REM[U], 
  // MULW, DIV[U]W, REM[U]W (only for RV64M)

  logic MulDivFlg, MulDivNoWFlg, MulDivWFlg;
  assign MulDivFlg    = intRs2InsFlg & f7_0000001;
  assign MulDivNoWFlg = intNoWRs2InsFlg & f7_0000001;
  assign MulDivWFlg   = intWRs2InsFlg & f7_0000001;

  function automatic logic MulDivIns(); // Mul & Div instructions valid
    return (MulDivFlg);
  endfunction: MulDivIns

  function automatic logic MulDivWIns(); // Mul & Div .W instructions valid
    return (MulDivWFlg);
  endfunction: MulDivWIns

  function automatic logic MulIns(); // Mul instructions valid
    return (MulDivFlg & ~funct3[2]);
  endfunction: MulIns

  function automatic logic DivIns(); // Div instructions valid
    return (MulDivFlg & funct3[2]);
  endfunction: DivIns

  // TODO: ins type divide 

  function automatic logic Mul(); // MUL valid
    return (MulDivNoWFlg & f3Oh[0]);
  endfunction: Mul

  function automatic logic Mulh(); // MULH valid
    return (MulDivNoWFlg & f3Oh[1]);
  endfunction: Mulh

  function automatic logic Mulhsu(); // MULHSU valid
    return (MulDivNoWFlg & f3Oh[2]);
  endfunction: Mulhsu

  function automatic logic Mulhu(); // MULHU valid
    return (MulDivNoWFlg & f3Oh[3]);
  endfunction: Mulhu

  function automatic logic Div(); // DIV valid
    return (MulDivNoWFlg & f3Oh[4]);
  endfunction: Div

  function automatic logic Divu(); // DIVU valid
    return (MulDivNoWFlg & f3Oh[5]);
  endfunction: Divu

  function automatic logic Rem(); // REM valid
    return (MulDivNoWFlg & f3Oh[6]);
  endfunction: Rem

  function automatic logic Remu(); // REMU valid
    return (MulDivNoWFlg & f3Oh[7]);
  endfunction: Remu

  function automatic logic Mulw(); // MULW valid
    return (MulDivWFlg & f3Oh[0]);
  endfunction: Mulw

  function automatic logic Divw(); // DIVW valid
    return (MulDivWFlg & f3Oh[4]);
  endfunction: Divw

  function automatic logic Divuw(); // DIVUW valid
    return (MulDivWFlg & f3Oh[5]);
  endfunction: Divuw

  function automatic logic Remw(); // REMW valid
    return (MulDivWFlg & f3Oh[6]);
  endfunction: Remw

  function automatic logic Remuw(); // REMUW valid
    return (MulDivWFlg & f3Oh[7]);
  endfunction: Remuw
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // System instructions: FENCE, FENCE.I, ECALL, EBREAK and CSR instructions
  function automatic logic FenceIns(); // Fence instructions valid: FENCE, FENCE.I
    return (fenceInsFlg);
  endfunction: FenceIns

  function automatic logic Fence(); // FENCE valid
    return (fenceInsFlg & ~f3Oh[0]);
  endfunction: Fence

  function automatic logic Fencei(); // FENCE.I valid
    return (fenceIFlg);
  endfunction: Fencei

  function automatic logic Ecall(); // ECALL valid
    return (f7_0000000 & (rs2==5'b00000) & (rs1==5'b00000) & (rd==5'b00000) & sysInsFlg);
  endfunction: Ecall

  function automatic logic Ebreak(); // EBREAK valid
    return (f7_0000000 & (rs2==5'b00001) & (rs1==5'b00000) & (rd==5'b00000) & sysInsFlg);
  endfunction: Ebreak

  // CSR instructions: CSRRW[I], CSRRS[I], CSRRC[I]---------------------------------------------------------------------
  function automatic logic CsrIns(); // CSR instructions valid
    return (sysInsFlg & ~f3Oh[0]);
  endfunction: CsrIns

  function automatic logic CsrImmIns(); // CSR with Imm valid
    return (csrUimmFlg);
  endfunction: CsrImmIns

  function automatic logic [1:0] CsrOp(); // CSR operation type
    return ({2{sysInsFlg}} & funct3[1:0]);
  endfunction: CsrOp

  function automatic logic CsrNoRead(); // CSR operation with no read
    return (sysInsFlg & (funct3[1:0]==2'b01) & (rd==5'b00000));
  endfunction: CsrNoRead

  function automatic logic CsrNoWrite(); // CSR operation with no write
    return (sysInsFlg & funct3[1] & (rs1==5'b00000));
  endfunction: CsrNoWrite

  function automatic logic Csrrw(); // CSRRW valid
    return (sysInsFlg & f3Oh[1]);
  endfunction: Csrrw

  function automatic logic Csrrs(); // CSRRS valid
    return (sysInsFlg & f3Oh[2]);
  endfunction: Csrrs

  function automatic logic Csrrc(); // CSRRC valid
    return (sysInsFlg & f3Oh[3]);
  endfunction: Csrrc

  function automatic logic Csrrwi(); // CSRRWI valid
    return (sysInsFlg & f3Oh[5]);
  endfunction: Csrrwi

  function automatic logic Csrrsi(); // CSRRSI valid
    return (sysInsFlg & f3Oh[6]);
  endfunction: Csrrsi

  function automatic logic Csrrci(); // CSRRCI valid
    return (sysInsFlg & f3Oh[7]);
  endfunction: Csrrci
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  // Atomic instructions:
  // TODO: add Atomic ins
  //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

endinterface: ZionRiscvIsaLib_RvimazDecodeItf
`endif


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interface name : ZionRiscvIsaLib_LoadExItf
// Author         : Wenheng Ma
// Date           : 2019-08-02
// Version        : 1.0
// Description    :
//   Define signals that load ISAs nead. And offer an Excution function to get the load operation result.
//   Load operation contains 7 instructions: 
//     - LB/LBU, LH/LHU, LW (R32I)
//     - LWU, LD            (R64I)
//   Enables for each load ins is combined in loadEn:
//     loadEn[0]-LB[U]   loadEn[1]-LH[U]   loadEn[2]-LW[U]   loadEn[3]-LD
//   Parameter RV64 indicate whether the processor is 64-bit core with the ISA of RV64I.
// Modification History:
//   Date   |   Author   |   Version   |   Change Description
//======================================================================================================================
// 19-08-02 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
`ifndef Disable_ZionRiscvIsaLib_LoadExItf
interface ZionRiscvIsaLib_LoadExItf
#(RV64 = 0);

  localparam CPU_WIDTH = 32*(RV64+1);
  logic [(2+RV64):0] loadEn;
  logic              unsignedLoad;
  logic [(1+RV64):0] addr;
  logic [CPU_WIDTH-1:0] memDat;

  function automatic logic [CPU_WIDTH-1:0] Exec;
    
    logic [CPU_WIDTH-1:0] result;
    // LB 
    logic [CPU_WIDTH/8-1:0][7:0] byteSplitDat;
    logic                  [7:0] byteLoadDat;
    logic                        byteMsb,byteExtend;
    logic [CPU_WIDTH-1  :0]      byteLoadRslt;
    // LH
    logic [CPU_WIDTH/16-1:0][15:0] halfwordSplitDat;
    logic                   [15:0] halfwordLoadDat;
    logic                          halfwordMsb,halfwordExtend;
    logic [CPU_WIDTH-1   :0]       halfwordLoadRslt;
    // LW
    logic [CPU_WIDTH/32-1:0][31:0] wordSplitDat;
    logic                   [31:0] wordLoadDat;
    logic                          wordMsb,wordExtend;
    logic [CPU_WIDTH-1   :0]       wordLoadRslt;
    // LD
    logic [CPU_WIDTH-1   :0] doubleLoadRslt;
    // LB
    byteSplitDat = memDat;
    byteLoadDat  = byteSplitDat[addr];//[addr[$high(addr):0]]
    byteExtend   = (~unsignedLoad) & byteLoadDat[$high(byteLoadDat)];
    byteLoadRslt = {CPU_WIDTH{loadEn[0]}} & {{(CPU_WIDTH-$bits(byteLoadDat)){byteExtend}}, byteLoadDat};
    // LH
    halfwordSplitDat = memDat;
    halfwordLoadDat  = halfwordSplitDat[addr[$high(addr)]];//addr[$high(addr):1]
    halfwordExtend   = (~unsignedLoad) & halfwordLoadDat[$high(halfwordLoadDat)];
    halfwordLoadRslt = {CPU_WIDTH{loadEn[1]}} & {{(CPU_WIDTH-$bits(halfwordLoadDat)){halfwordExtend}}, halfwordLoadDat};
    `gen_if(RV64==0)begin
      // LW for RV32
      wordLoadRslt = {CPU_WIDTH{loadEn[2]}} & memDat;
      result = byteLoadRslt | halfwordLoadRslt | wordLoadRslt;
    end `gen_else begin//`gen_elif(RV64==1)
      // LW for RV64
      wordSplitDat = memDat;
      wordLoadDat  = wordSplitDat[addr[$high(addr)]];//[addr[$high(addr):2]]
      wordExtend   = (~unsignedLoad) & wordLoadDat[$high(wordLoadDat)];
      wordLoadRslt = {CPU_WIDTH{loadEn[2]}} & {{(CPU_WIDTH-$bits(wordLoadDat)){wordExtend}}, wordLoadDat};
      //LD for RV64
      doubleLoadRslt = {CPU_WIDTH{loadEn[3]}} & memDat;
      result = byteLoadRslt | halfwordLoadRslt | wordLoadRslt | doubleLoadRslt;
    end
    return result;

  endfunction: Exec

  modport De (output loadEn, unsignedLoad, addr, memDat);
  modport Ex (input  loadEn, unsignedLoad, addr, memDat, import Exec);

endinterface: ZionRiscvIsaLib_LoadExItf
`endif



///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interface name : ZionRiscvIsaLib_StoreExItf
// Author         : Wenheng Ma
// Date           : 2019-08-02
// Version        : 1.0
// Description    :
//   Define signals that store ISAs nead. And offer an Excution function to get the load operation result.
//   Store operation contains  instructions: SB, SH, SW, SD(only for R64I). 
//   Enables for each store ins is combined in storeEn:
//     storeEn[0]-SB   storeEn[1]-SH   storeEn[2]-SW   storeEn[3]-SD
//   Parameter RV64 indicate whether the processor is 64-bit core with the ISA of RV64I.
// Modification History:
//   Date   |   Author   |   Version   |   Change Description
//======================================================================================================================
// 19-08-02 | Wenheng Ma |     1.0     |   Original Version
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
`ifndef Disable_ZionRiscvIsaLib_StoreExItf
interface ZionRiscvIsaLib_StoreExItf
#(RV64 = 0);

  localparam CPU_WIDTH = 32*(RV64+1);
  localparam int STORE_WIDTH = {8,16,32};
  logic [(2+RV64):0] storeEn;
  logic [(1+RV64):0] addr;
  logic [CPU_WIDTH-1:0] storeDat;

  function automatic logic [CPU_WIDTH-1:0] Exec;
    
    typedef logic [CPU_WIDTH  -1:0] type_WrDat;
    typedef logic [CPU_WIDTH/8-1:0] type_WrEn;
    logic [CPU_WIDTH  -1:0] wrDat;
    logic [CPU_WIDTH/8-1:0] wrEn;
    //SB
    logic [CPU_WIDTH/8-1:0][7:0] byteWrDat;
    logic [CPU_WIDTH/8-1:0]      byteWrEn;
    //SH
    logic [CPU_WIDTH/16-1:0][15:0] halfwordWrDat;
    logic [CPU_WIDTH/16-1:0][ 1:0] halfwordWrEn;
    //SW
    logic [CPU_WIDTH/32-1:0][31:0] wordWrDat;
    logic [CPU_WIDTH/32-1:0][ 3:0] wordWrEn;
    //SD
    logic [CPU_WIDTH/-1:0] doubleWrDat;
    logic [           7:0] doubleWrEn;
    //SB
    byteWrDat = '0;
    byteWrEn  = '0;
    byteWrDat[addr[$high(addr):0]] = {8{storeEn[0]}} & storeDat[0+:8];
    byteWrEn[addr[$high(addr):0]]  = storeEn[0];
    //SH
    halfwordWrDat = '0;
    halfwordWrEn  = '0;
    halfwordWrDat[addr[$high(addr):1]] = {16{storeEn[1]}} & storeDat[0+:16];
    halfwordWrEn[addr[$high(addr):1]]  = {2{storeEn[1]}};
    `gen_if(RV64==0) begin
      //SW for RV32
      wordWrDat = {31storeEn[2]} & storeDat;
      wordWrEn  = {4{storeEn[2]}};
      wrDat = type_WrDat'(byteWrDat) | type_WrDat'(halfwordWrDat) | wordWrDat;
      wrEn  = type_WrEn'(byteWrEn)   | type_WrEn'(halfwordWrEn)   | wordWrEn;
    end `gen_elif(RV64==1) begin
      //SW for RV64
      wordWrDat = '0;
      wordWrEn  = '0;
      wordWrDat[addr[$high(addr):2]] = {32{storeEn[2]}} & storeDat[0+:32];
      wordWrEn[addr[$high(addr):2]]  = {4{storeEn[2]}};
      //SD for RV64
      doubleWrDat = {31storeEn[3]} & storeDat;
      doubleWrEn  = {8{storeEn[3]}};
      wrDat = type_WrDat'(byteWrDat) | type_WrDat'(halfwordWrDat) | type_WrDat'(wordWrDat) | doubleWrDat;
      wrEn  = type_WrEn'(byteWrEn)   | type_WrEn'(halfwordWrEn)   | type_WrEn'(wordWrEn)   | doubleWrEn;
    end

    return {rsltMask, rsltDat};

  endfunction: Exec

  modport De (output storeEn, addr, memDat);
  modport Ex (input  storeEn, addr, memDat, import Exec);

endinterface: ZionRiscvIsaLib_StoreExItf
`endif
