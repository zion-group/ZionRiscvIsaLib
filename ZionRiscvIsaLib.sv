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
  logic [CPU_WIDTH-1:0] shamt, immItype, immStype, immBtype, immUtype, immJtype, uimm;
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
    foreach(f3Oh[i])  f3Oh[i] = (funct3 == i);
    f7h6_000000 = (funct7[6:1] == 6'b000000);
    f7h6_010000 = (funct7[6:1] == 6'b010000);
    f7_0000000  = f7h6_000000 & ~funct7[0];
    f7_0000001  = f7h6_000000 &  funct7[0];
    f7_0100000  = f7h6_010000 & ~funct7[0];
  end

  logic notRvc, bigImmInsFlg, opHigh_110xx, bjInsFlg, jumpInsFlg, branchInsFlg;
  logic loadStoreInsFlg, loadInsFlg, storeInsFlg;
  logic intInsFlg, intImmInsFlg, intRs2InsFlg, wInsFLg;
  logic wInsFlg,sysInsFlg, atomicInsFlg, fenceInsFlg;
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
  endfunction: BigImmIns

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
    return (bjInsFlg);
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
  endfunction:LoadStoreWidth

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
  endfunction: XorIns

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
    return ((funct3[1:0]==2'b01) & ((intImmInsFlg) | (intRs2InsFlg & (f7_0000000 | f7_0100000))));
  endfunction: SftIns

  function automatic logic SftWIns(); // W type shift valid: SLL[I]W, SRL[I]W, SRA[I]W
    return ((funct3[1:0]==2'b01) & ((intWImmInsFlg) | (intWRs2InsFlg & (f7_0000000 | f7_0100000))));
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
  endfunction: Slti

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
  modport Ex (input  op, s1, s2);
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
  `define ZionRiscvIsaLib_AddSubExec(UnitName,iAddSubExIf_MT,oRslt_MT)   \
// `ifdef VIVADO_SYN                                                        \
//     localparam UnitName``_RV64 = iAddSubExIf_MT.RV64;                    \
//   `else                                                                  \
//     localparam UnitName``_RV64 = $bits(iAddSubExIf_MT.s1)/32-1;          \
//   `endif                                                                 \
  ZionRiscvIsaLib_AddSubExec#(.CPU_WIDTH($bits(oRslt_MT)))               \
                            UnitName(                                    \
                              .iAddSubExIf(iAddSubExIf_MT),              \
                              .oRslt(oRslt_MT)                           \
                            )
`endif
module ZionRiscvIsaLib_AddSubExec
#(CPU_WIDTH = 32
)(
  ZionRiscvIsaLib_AddSubExItf.Ex iAddSubExIf,
  output logic [CPU_WIDTH-1:0] oRslt
);
 `Use_ZionBasicCircuitLib(Bc)
  localparam RV64 = (CPU_WIDTH/32)-1;
  logic [CPU_WIDTH-1:0] s1, s1Tmp, s2, s2Tmp, rsltTmp;
  wire addEn = iAddSubExIf.op[0];
  wire subEn = iAddSubExIf.op[1];
  always_comb begin
    s1      = iAddSubExIf.s1;
    s2      = iAddSubExIf.s2;
    rsltTmp = `BcAddSubdM(addEn,subEn,s1,s2);
  end
  `gen_if(RV64) begin : Rv64RsltGen
    wire WFlg = iAddSubExIf.op[2];
    assign oRslt = (WFlg) ? {{32{rsltTmp[31]}},rsltTmp[31:0]} : rsltTmp;
  end `gen_else begin : Rv32RsltGen
    assign oRslt = rsltTmp;
  end

  // Only one kind of operation can be done in a certain cycle. If both of addEn(iAddSubExIf.op[0])
  // and subEn(iAddSubExIf.op[1]) is 1, the result will be undifined and lead to an error. So it is
  // necessary to assert the situation.
 always_comb begin
    assert ($onehot0({addEn, subEn}))
    else $error("Signal Error: Both of addEn and subEn are activated which only one could work at a certain time.");
  end

 `Unuse_ZionBasicCircuitLib(Bc)
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



//section: BitOperationEx ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Circuits about bit operation instructions are provided in this section.
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interface name : ZionRiscvIsaLib_BitsExItf
// Author         : Wenheng Ma
// Date           : 2019-08-02
// Version        : 1.0
// Parameter      :
//   RV64 - indicate whether the circuit is for RV64 or not. 1:RV64.  2:RV32. It can be inferred from iSftExIf.
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
  modport Ex (input  andEn, orEn, xorEn, s1, s2, import Exec);

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
  `define ZionRiscvIsaLib_BitsOpExec(UnitName,iBitsExif_MT,oRslt_MT) \
ZionRiscvIsaLib_BitsOpExec  #(.CPU_WIDTH($bits(oRslt_MT)))           \
                            UnitName(                                \
                              .iBitsExif(iBitsExif_MT),              \
                              .oRslt(oRslt_MT)                       \
                            )
`endif
module ZionRiscvIsaLib_BitsOpExec
#(CPU_WIDTH = 32
)(
  ZionRiscvIsaLib_BitsExItf.Ex iBitsExif,
  output logic [CPU_WIDTH-1:0] oRslt
);

  always_comb begin
    oRslt   = iBitsExif.Exec();
  end

  // Only one kind of operation can be done in a certain cycle. If more than 1 'xxEn' signals are acctivated,
  // the result will be undifined and lead to an error. So it is necessary to assert the situation.
  always_comb begin
    assert($onehot0({iBitsExif.andEn, iBitsExif.orEn, iBitsExif.xorEn})) 
    else $error("Signal Error: More than 1 'xxEn' signals are activated in iBitsExif.andEn, iBitsExif.orEn and iBitsExif.xorEn which only one could work.");
  end

endmodule: ZionRiscvIsaLib_BitsOpExec
`endif

//section: BitOperationEx ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



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
 `Use_ZionBasicCircuitLib(Bc)
  localparam CPU_WIDTH = 32*(RV64+1);
  logic bjIns, branch, beq, bne, blt, bge, unsignedFlg, jump;
  logic [1:0] linkOffset;
  logic [CPU_WIDTH-1:0] pc, s1, s2, offset, tgtAddr, linkPc, s1Fnl;

  // Calculate branch&jump target address.
  // For Jal instruction, the PC must put into s1 before execution.
  function automatic logic [CPU_WIDTH-1:0] TgtAddrGen;

    logic [CPU_WIDTH-1:0] baseAddr, tgtOffset;
    baseAddr  =  `BcMaskM(branch,pc)     
                |`BcMaskM(jump,s1);      
    tgtOffset =  `BcMaskM(bjIns,offset);  
    return (baseAddr + tgtOffset);

  endfunction : TgtAddrGen

  // Calculate less than for Blt Bge instructions.
  // Extend data according to unsigned flag(unsignedFlg), then compare the size of s1 and s2.
  // With the extension of data, we can reuse the compare circuits for both signed and unsigned compare operations.
  function automatic logic LessThan;

    logic signed [CPU_WIDTH:0] s1Extd, s1Mask, s2Extd, s2Mask;
    // s1Extd = {((~unsignedFlg) & s1[CPU_WIDTH-1]) , s1}; // Extend s1 according unsignedFld // TODO: use HighBit
    // s2Extd = {((~unsignedFlg) & s2[CPU_WIDTH-1]) , s2}; // Extend s2 according unsignedFld// TODO: use HighBit
    // s1Mask = {$bits(s1Extd){branch}} & s1Extd; // TODO: use Mask
    // s2Mask = {$bits(s2Extd){branch}} & s2Extd; // TODO: use Mask
    s1Extd = {((~unsignedFlg) & `BcHighB(s1)) , s1}; 
    s2Extd = {((~unsignedFlg) & `BcHighB(s2)) , s2};
    s1Mask =`BcMaskM(branch,s1Extd);
    s1Mask =`BcMaskM(branch,s2Extd);
    return ((s1Mask<s2Mask)? 1'b1:1'b0);

  endfunction : LessThan

  // Calculate the link pc. 
  function automatic logic [CPU_WIDTH-1:0] LinkPcGen; 
    return (`BcMaskM(jump,pc) + ({linkOffset,1'b0}));
  endfunction : LinkPcGen

  // To reuse adder for generating link pc, the function works as a mux that selects s1 or link offset.
  // But it is implemented by 'bit or' for high bits. So it has a better performance.
  function automatic logic [CPU_WIDTH-1:0] S1LinkOffsetMux;
    logic [CPU_WIDTH-1:0] s1Mask, muxRslt;
    s1Mask = `BcMaskM((~jump),s1);
    muxRslt[CPU_WIDTH-1:3] = s1Mask[CPU_WIDTH-1:3];
    muxRslt[2:1] = s1Mask[2:1] | linkOffset;
    muxRslt[ 0 ] = s1Mask[ 0 ];
    return muxRslt;
  endfunction : S1LinkOffsetMux

  modport De( output bjIns, branch, beq, bne, blt, bge, unsignedFlg, jump, pc, s1, s2, offset, linkOffset);
  modport Ex( input  bjIns, branch, beq, bne, blt, bge, unsignedFlg, jump, pc, s1, s2, offset, linkOffset,
              import LessThan
            );
  `Unuse_ZionBasicCircuitLib(Bc)
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
  `define ZionRiscvIsaLib_BjTgtAddr(UnitName,iBjExIf_MT,oTgtAddr_MT)   \
ZionRiscvIsaLib_BjTgtAddr  #(.CPU_WIDTH($bits(oTgtAddr_MT)))           \
                          UnitName (                                   \
                             .iBjExIf(iBjExIf_MT),                     \
                             .oTgtAddr(oTgtAddr_MT)                    \
                             )
`endif
module ZionRiscvIsaLib_BjTgtAddr
#(CPU_WIDTH =32
 )(
  ZionRiscvIsaLib_BjExItf iBjExIf,
  output logic [CPU_WIDTH-1:0] oTgtAddr
);

  always_comb begin
    oTgtAddr  = iBjExIf.TgtAddrGen();
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
                              .iLessThan(iLessThan_MT),                             \
                              .oBjEn(oBjEn_MT)                                      \
                            )
`endif
module ZionRiscvIsaLib_BjEnNoLessThan
(
  ZionRiscvIsaLib_BjExItf.Ex iBjExIf,
  input                      iLessThan,
  output logic               oBjEn
);

  logic equal;
  always_comb begin
    equal    = (iBjExIf.s1 == iBjExIf.s2);
    oBjEn    = iBjExIf.jump 
              |(iBjExIf.beq & equal)
              |(iBjExIf.bne & !equal) 
              |(iBjExIf.blt & iLessThan)       // Blt  instuction lead to branch&jump
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
  output logic               oBjEn
);

  logic lessThan;
  always_comb begin
    lessThan = iBjExIf.LessThan();
  end
  ZionRiscvIsaLib_BjEnNoLessThan  U_ZionRiscvIsaLib_BjEnNoLessThan(
                                    .iBjExIf,
                                    .iLessThan(lessThan),
                                    .oBjEn(bjEn)
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
  `define ZionRiscvIsaLib_JumpLinkPc(UnitName,iBjExIf_MT,oLinkPc_MT) \
ZionRiscvIsaLib_JumpLinkPc  #(.CPU_WIDTH($bits(oLinkPc_MT)))         \
                            UnitName(                                \
                                .iBjExIf(iBjExIf_MT),                \
                                .oLinkPc(oLinkPc_MT)                 \
                              )
`endif
module ZionRiscvIsaLib_JumpLinkPc
#(CPU_WIDTH = 32
)(
  ZionRiscvIsaLib_BjExItf               iBjExIf,
  output logic [CPU_WIDTH-1:0]          oLinkPc
);

  always_comb begin
    oLinkPc = iBjExIf.LinkPcGen();
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
  `define ZionRiscvIsaLib_S1LinkOffsetMux(UnitName,iBjExIf_MT,ooS1Mux_MT)   \
ZionRiscvIsaLib_S1LinkOffsetMux #(.CPU_WIDTH($bits(ooS1Mux_MT)))            \
                                UnitName(                                   \
                                .iBjExIf(iBjExIf_MT),                       \
                                .oS1Mux(ooS1Mux_MT)                         \
                                )
`endif
module ZionRiscvIsaLib_S1LinkOffsetMux
#(CPU_WIDTH = 32
)(
  ZionRiscvIsaLib_BjExItf          iBjExIf,
  output logic  [CPU_WIDTH-1:0]    oS1Mux
);

  always_comb begin
    oS1Mux = iBjExIf.S1LinkOffsetMux();
  end

endmodule : ZionRiscvIsaLib_S1LinkOffsetMux
`endif

//endsection: BranchJumpEx +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



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
`Use_ZionBasicCircuitLib(Bc)

  localparam CPU_WIDTH = 32*(RV64+1);
  logic en, unsignedFlg, rslt;
  logic [CPU_WIDTH-1:0] s1,s2;

  // Extend data according to unsigned flag(unsignedFlg), then compare the size of s1 and s2.
  // With the extension of data, we can reuse the compare circuits for both signed and unsigned compare operations.
  function automatic logic Exec;

    logic signed [CPU_WIDTH:0] s1Extd, s1Mask, s2Extd, s2Mask;
    s1Extd = {((~unsignedFlg) & `BcHighB(s1)) , s1};
    s2Extd = {((~unsignedFlg) & `BcHighB(s2)) , s2};
    s1Mask = `BcMaskM(en,s1Extd);
    s2Mask = `BcMaskM(en,s2Extd);
    return ((s1Mask<s2Mask)? 1'b1:1'b0);

  endfunction: Exec

  modport De (output en, unsignedFlg, s1, s2);
  modport Ex (input  en, unsignedFlg, s1, s2, import Exec);

 `Unuse_ZionBasicCircuitLib(Bc)
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
  `define ZionRiscvIsaLib_SetLessThan(UnitName,iSltExIf_MT,oRslt_MT)      \
  ZionRiscvIsaLib_SetLessThan UnitName(                                   \
                                .iSltExIf(iSltExIf_MT),                   \
                                .oRslt(oRslt_MT)                          \
                              )
`endif
module ZionRiscvIsaLib_SetLessThan
(
  ZionRiscvIsaLib_SltExItf.Ex iSltExIf,
  output logic oRslt
);

  always_comb begin
    oRslt = iSltExIf.Exec();
  end

endmodule: ZionRiscvIsaLib_SetLessThan
`endif

//endsection: SetLessThanEx ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



//section: ShiftEx +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Circuits about shift instructions are provided in this section.
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interface name : ZionRiscvIsaLib_SftExItf
// Author         : Wenheng Ma
// Date           : 2019-10-27
// Version        : 1.0
// Parameter      :
//   RV64 - indicate whether the circuit is for RV64 or not. 1:RV64.  2:RV32. It can be inferred from iSftExIf.
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
  modport Ex (input  op, s1, s2);

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
  `define ZionRiscvIsaLib_SftExec(UnitName,iSftExIf_MT,oRslt_MT)      \
// `ifdef VIVADO_SYN                                                     \
//     localparam UnitName``_RV64 = iSftExIf_MT.RV64;                    \
//   `else                                                               \
//     localparam UnitName``_RV64 = $bits(iSftExIf_MT.s1)/32-1;          \
//   `endif                                                              \
  ZionRiscvIsaLib_SftExec   #(.CPU_WIDTH($bits(oRslt_MT)))            \
                            UnitName(                                 \
                              .iSftExIf(iSftExIf_MT),                 \
                              .oRslt(oRslt_MT)                        \
                            )
`endif
module ZionRiscvIsaLib_SftExec
#(CPU_WIDTH = 32
)(
  ZionRiscvIsaLib_SftExItf.Ex iSftExIf,
  output logic  [CPU_WIDTH-1:0] oRslt
);
`Use_ZionBasicCircuitLib(Bc)
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

  localparam RV64 = (CPU_WIDTH/32)-1;
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
    assign oRslt = rsltTmp;
  end

  `BcMultiTypeShift (U_MultiTypeShift,sftR,sftA,sftL,1'b0,sftDat,sftBits,rsltTmp);
  // Assertions for signal check.
  always_comb begin
    // There is no ShiftLeftArithmetic instruction, so sftA and sftL can not be activated simultaneously.
    assert ($onehot0({sftA, sftL}))  
    else $error("Signal Error: Both of sftA and sftL are activated which only one could work at a certain time.");
  end
  if(RV64==1) begin
    always_comb begin
      // For ShiftW instructions(SLLW/SRLW/SRAW), the highest bit in SftExIf.s2 is not used. 
      // Thus this bit must be 0 for ShiftW.
      assert ($onehot0({sftW, iSftExIf.s2[5]})) 
      else $error("Signal Error: Both of sftA and sftL are activated which only one could work at a certain time.");
    end
  end
  `Unuse_ZionBasicCircuitLib(Bc)
endmodule: ZionRiscvIsaLib_SftExec
`endif
//endsection: ShiftEx ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



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
// `ifndef Disable_ZionRiscvIsaLib_LoadExItf
// interface ZionRiscvIsaLib_LoadExItf
// #(RV64 = 0);

//   localparam CPU_WIDTH = 32*(RV64+1);
//   logic [(2+RV64):0] loadEn;
//   logic              unsignedLoad;
//   logic [(1+RV64):0] addr;
//   logic [CPU_WIDTH-1:0] memDat;

//   function automatic logic [CPU_WIDTH-1:0] Exec;
    
//     logic [CPU_WIDTH-1:0] result;
//     // LB 
//     logic [CPU_WIDTH/8-1:0][7:0] byteSplitDat;
//     logic                  [7:0] byteLoadDat;
//     logic                        byteMsb,byteExtend;
//     logic [CPU_WIDTH-1  :0]      byteLoadRslt;
//     // LH
//     logic [CPU_WIDTH/16-1:0][15:0] halfwordSplitDat;
//     logic                   [15:0] halfwordLoadDat;
//     logic                          halfwordMsb,halfwordExtend;
//     logic [CPU_WIDTH-1   :0]       halfwordLoadRslt;
//     // LW
//     logic [CPU_WIDTH/32-1:0][31:0] wordSplitDat;
//     logic                   [31:0] wordLoadDat;
//     logic                          wordMsb,wordExtend;
//     logic [CPU_WIDTH-1   :0]       wordLoadRslt;
//     // LD
//     logic [CPU_WIDTH-1   :0] doubleLoadRslt;
//     // LB
//     byteSplitDat = memDat;
//     byteLoadDat  = byteSplitDat[addr];//[addr[$high(addr):0]]
//     byteExtend   = (~unsignedLoad) & byteLoadDat[$high(byteLoadDat)];
//     byteLoadRslt = {CPU_WIDTH{loadEn[0]}} & {{(CPU_WIDTH-$bits(byteLoadDat)){byteExtend}}, byteLoadDat};
//     // LH
//     halfwordSplitDat = memDat;
//     halfwordLoadDat  = halfwordSplitDat[addr[$high(addr)]];//addr[$high(addr):1]
//     halfwordExtend   = (~unsignedLoad) & halfwordLoadDat[$high(halfwordLoadDat)];
//     halfwordLoadRslt = {CPU_WIDTH{loadEn[1]}} & {{(CPU_WIDTH-$bits(halfwordLoadDat)){halfwordExtend}}, halfwordLoadDat};
//     `gen_if(RV64==0)begin
//       // LW for RV32
//       wordLoadRslt = {CPU_WIDTH{loadEn[2]}} & memDat;
//       result = byteLoadRslt | halfwordLoadRslt | wordLoadRslt;
//     end `gen_else begin//`gen_elif(RV64==1)
//       // LW for RV64
//       wordSplitDat = memDat;
//       wordLoadDat  = wordSplitDat[addr[$high(addr)]];//[addr[$high(addr):2]]
//       wordExtend   = (~unsignedLoad) & wordLoadDat[$high(wordLoadDat)];
//       wordLoadRslt = {CPU_WIDTH{loadEn[2]}} & {{(CPU_WIDTH-$bits(wordLoadDat)){wordExtend}}, wordLoadDat};
//       //LD for RV64
//       doubleLoadRslt = {CPU_WIDTH{loadEn[3]}} & memDat;
//       result = byteLoadRslt | halfwordLoadRslt | wordLoadRslt | doubleLoadRslt;
//     end
//     return result;

//   endfunction: Exec

//   modport De (output loadEn, unsignedLoad, addr, memDat);
//   modport Ex (input  loadEn, unsignedLoad, addr, memDat, import Exec);

// endinterface: ZionRiscvIsaLib_LoadExItf
// `endif



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
// `ifndef Disable_ZionRiscvIsaLib_StoreExItf
// interface ZionRiscvIsaLib_StoreExItf
// #(RV64 = 0);

//   localparam CPU_WIDTH = 32*(RV64+1);
//   localparam int STORE_WIDTH = {8,16,32};
//   logic [(2+RV64):0] storeEn;
//   logic [(1+RV64):0] addr;
//   logic [CPU_WIDTH-1:0] storeDat;

//   function automatic logic [CPU_WIDTH-1:0] Exec;
    
//     typedef logic [CPU_WIDTH  -1:0] type_WrDat;
//     typedef logic [CPU_WIDTH/8-1:0] type_WrEn;
//     logic [CPU_WIDTH  -1:0] wrDat;
//     logic [CPU_WIDTH/8-1:0] wrEn;
//     //SB
//     logic [CPU_WIDTH/8-1:0][7:0] byteWrDat;
//     logic [CPU_WIDTH/8-1:0]      byteWrEn;
//     //SH
//     logic [CPU_WIDTH/16-1:0][15:0] halfwordWrDat;
//     logic [CPU_WIDTH/16-1:0][ 1:0] halfwordWrEn;
//     //SW
//     logic [CPU_WIDTH/32-1:0][31:0] wordWrDat;
//     logic [CPU_WIDTH/32-1:0][ 3:0] wordWrEn;
//     //SD
//     logic [CPU_WIDTH/-1:0] doubleWrDat;
//     logic [           7:0] doubleWrEn;
//     //SB
//     byteWrDat = '0;
//     byteWrEn  = '0;
//     byteWrDat[addr[$high(addr):0]] = {8{storeEn[0]}} & storeDat[0+:8];
//     byteWrEn[addr[$high(addr):0]]  = storeEn[0];
//     //SH
//     halfwordWrDat = '0;
//     halfwordWrEn  = '0;
//     halfwordWrDat[addr[$high(addr):1]] = {16{storeEn[1]}} & storeDat[0+:16];
//     halfwordWrEn[addr[$high(addr):1]]  = {2{storeEn[1]}};
//     `gen_if(RV64==0) begin
//       //SW for RV32
//       wordWrDat = {31storeEn[2]} & storeDat;
//       wordWrEn  = {4{storeEn[2]}};
//       wrDat = type_WrDat'(byteWrDat) | type_WrDat'(halfwordWrDat) | wordWrDat;
//       wrEn  = type_WrEn'(byteWrEn)   | type_WrEn'(halfwordWrEn)   | wordWrEn;
//     end `gen_elif(RV64==1) begin
//       //SW for RV64
//       wordWrDat = '0;
//       wordWrEn  = '0;
//       wordWrDat[addr[$high(addr):2]] = {32{storeEn[2]}} & storeDat[0+:32];
//       wordWrEn[addr[$high(addr):2]]  = {4{storeEn[2]}};
//       //SD for RV64
//       doubleWrDat = {31storeEn[3]} & storeDat;
//       doubleWrEn  = {8{storeEn[3]}};
//       wrDat = type_WrDat'(byteWrDat) | type_WrDat'(halfwordWrDat) | type_WrDat'(wordWrDat) | doubleWrDat;
//       wrEn  = type_WrEn'(byteWrEn)   | type_WrEn'(halfwordWrEn)   | type_WrEn'(wordWrEn)   | doubleWrEn;
//     end

//     return {rsltMask, rsltDat};

//   endfunction: Exec

//   modport De (output storeEn, addr, memDat);
//   modport Ex (input  storeEn, addr, memDat, import Exec);

// endinterface: ZionRiscvIsaLib_StoreExItf
// `endif



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
  logic sltEn, bjIns, branch, beq, bne, blt, bge, jump;
  logic sftLeft, sftRight, sftA;
  logic memEn;
  logic bjEn;
  logic [1:0] linkOffset;
  logic [CPU_WIDTH-1:0] intRslt, BjTgt, memAddr;

  // modport for integer instructions excution with branch&jump and memory address calculate.
  modport IntBjMemDeOut (output pc, s1, s2, offset, flags, addEn, subEn, andEn, orEn, xorEn,
                                sftLeft, sftRight, sftA, sltEn,
                                addSubIns, bjIns, branch, beq, bne, blt, bge, jump, linkOffset, 
                                memEn
                        );
  modport IntBjMemExIn  (input  pc, s1, s2, offset, flags, addEn, subEn, andEn, orEn, xorEn,
                                sftLeft, sftRight, sftA, sltEn,
                                addSubIns, bjIns, branch, beq, bne, blt, bge, jump, linkOffset,
                                memEn
                        );
  modport IntBjMemExOut(output  intRslt, bjEn, BjTgt, memAddr);

  // modport for integer instructions excution with branch&jump.
  modport IntBjDeOut(output pc, s1, s2, offset, flags, addEn, subEn, andEn, orEn, xorEn,
                            sftLeft, sftRight, sftA, sltEn,
                            addSubIns, bjIns, branch, beq, bne, blt, bge, jump, linkOffset
                    );
  modport IntBjExIn (input  pc, s1, s2, offset, flags, addEn, subEn, andEn, orEn, xorEn,
                            sftLeft, sftRight, sftA, sltEn,
                            addSubIns, bjIns, branch, beq, bne, blt, bge, jump ,linkOffset
                    );
  modport IntBjExOut(output  intRslt, bjEn, BjTgt);

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
    wire lessThanFlg = `BcHighB(addSubRslt);
    `ZionRiscvIsaLib_S1LinkOffsetMux (U_S1LinkOffsetMux, BjIf.Ex, S1LinkOffsetMuxRlst);
    ZionRiscvIsaLib_BjExItf#(RV64) BjIf();
    always_comb begin
      AddSubIf.s1    = S1LinkOffsetMuxRlst;

      BjIf.bjEn        = iDat.bjIns;
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

      oDat.intRslt = BitsRslt  | `BcMaskM((iDat.addSubIns|iDat.jump),addSubRslt) 
                    |SftRslt   | `BcZeroExtdM((sltRslt & iDat.sltEn),CPU_WIDTH);
      oDat.BjTgt   = BjTgtAddrRlst;
    end
    `ZionRiscvIsaLib_AddSubLessThan (U_AddSubLessThan, AddSubIf, unsignedFlg, lessThanFlg, sltRslt);
    `ZionRiscvIsaLib_BjEnNoLessThan (U_BjEnNoLt , BjIf.Ex, lessThanFlg, oDat.bjEn );
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


