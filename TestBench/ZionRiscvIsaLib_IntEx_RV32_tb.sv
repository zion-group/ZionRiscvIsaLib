module ZionRiscvIsaLib_IntEx_RV32_tb;
`Use_ZionRiscvIsaLib(Rvi)
  parameter RV64=0;
  parameter INT_MODULE_TYPE = 2;
  parameter CPU_WIDTH = 32*(RV64+1);  
  parameter half_period = 5;
  `RviIntInsExItf #(RV64) iIntInsEx();
  `RviIntInsExItf #(RV64) oIntInsEx();
  logic                        clk;
  logic        [3:0]           OpSel;
  logic                        addsubSel;
  logic                        bitSel;
  logic                        sftSel;
  logic                        sltSel;
  logic                        bjSel;
  logic                        memSel;
  logic                        addsubOp;
  logic                        LessThan;
  logic                        equal;
  logic        [CPU_WIDTH-1:0] sltout;
  logic        [1:0]           BjEnout;
  logic        [1:0]           bitOp;
  logic        [1:0]           sftOp;
  logic        [3:0]           bjOp;
  logic        [CPU_WIDTH-1:0] tgtAddrout;
  logic signed [CPU_WIDTH  :0] signs1;
  logic signed [CPU_WIDTH  :0] signs2;
  logic        [CPU_WIDTH-1:0] linkpc;
  logic        [CPU_WIDTH-1:0] addsubout;
  logic        [CPU_WIDTH-1:0] bitout;
  logic        [CPU_WIDTH-1:0] sftout;
  logic                        offsel;
  logic        [CPU_WIDTH-1:0] memAddrout;
  logic        [CPU_WIDTH-1:0] linkOffset;
  logic        [CPU_WIDTH-1:0] out;
  logic        [2:0]           linkOff;
  logic        [CPU_WIDTH-1:0] tgtAddr;
  logic        [CPU_WIDTH-1:0] memAddr;
  
  initial begin
    clk = 0;
    forever #10 clk = ~clk;
  end
  initial begin
  	OpSel = 0;
  	addsubOp = 0;
  	bitOp = 0;
    sftOp = 0;
    bitOp = 0;
    iIntInsEx.s1 = 0;
    iIntInsEx.s2 = 0;
    iIntInsEx.pc = 0;
    iIntInsEx.flags  = 0;
    iIntInsEx.offset = 0;
    // iIntInsEx.pc     = 0;
    offsel    = 0;
    forever @(negedge clk) begin
      OpSel           = {$random()}%7;
      addsubOp        = {$random()}%2;
      bitOp           = {$random()}%3;
      sftOp           = {$random()}%3;
      bjOp            = {$random()}%5;
      iIntInsEx.flags = {$random()}%2;
      offsel          = {$random()}%2;
      iIntInsEx.s1 =  {$random()}%(CPU_WIDTH+1);
      iIntInsEx.s2 =  {$random()}%(CPU_WIDTH+1);
      iIntInsEx.pc = {$random()}%(CPU_WIDTH+1);
      iIntInsEx.offset = {$random()}%(CPU_WIDTH+1);
      // iIntInsEx.pc     = {$random()}%(CPU_WIDTH+1);      
    end
  end 

  always_comb begin
    case(offsel)
       'd0:   linkOffset = 2'b01;  
       'd1:   linkOffset = 2'b10; //full_case
    endcase // offsel
  end

`gen_if(INT_MODULE_TYPE==0) begin: IntModuleType_0
  always_comb begin
  	case(OpSel)
  		 'd0:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b10_0000;
  		 'd1:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b01_0000; 
  		 'd2:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_1000;
  		 'd3:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0100;
  		 'd4:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0010;
  		 'd5:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0001; 
    default: {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0000;
  	endcase // OpSel
  end 
  end
`gen_elif(INT_MODULE_TYPE==1) begin
   always_comb begin
    case(OpSel)
       'd0:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b10_0000;
       'd1:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b01_0000; 
       'd2:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_1000;
       'd3:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0100;
       'd4:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0010;
       'd5:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0000; 
    default: {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0000;
    endcase // OpSel
  end  
  end
`gen_elif(INT_MODULE_TYPE==2) begin
     always_comb begin
    case(OpSel)
       'd0:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b10_0000;
       'd1:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b01_0000; 
       'd2:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_1000;
       'd3:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0100;
       'd4:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0000;
       'd5:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0001; 
    default: {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b00_0000;
    endcase // OpSel
  end  
  end
  
  always_comb begin
  	{iIntInsEx.addEn,     iIntInsEx.subEn,    iIntInsEx.andEn, iIntInsEx.orEn,  iIntInsEx.xorEn} = 'd0;
  	{iIntInsEx.sftLeft,   iIntInsEx.sftRight, iIntInsEx.sftA,  iIntInsEx.sltEn, iIntInsEx.memEn} = 'd0;
  	{iIntInsEx.addSubIns, iIntInsEx.bjEn,     iIntInsEx.branch,iIntInsEx.beq,   iIntInsEx.bne, iIntInsEx.blt, iIntInsEx.bge, iIntInsEx.jump} = 'd0;
   if (addsubSel) begin
  	  case(addsubOp)
  		     'd0: {iIntInsEx.addSubIns,iIntInsEx.addEn,iIntInsEx.subEn} = 3'b110;
  		     'd1: {iIntInsEx.addSubIns,iIntInsEx.addEn,iIntInsEx.subEn} = 3'b101;
       default: {iIntInsEx.addSubIns,iIntInsEx.addEn,iIntInsEx.subEn} = 3'b000;
  	  endcase // addsubOp
   end else if (bitSel) begin
   	  case(bitOp)
  		     'd0: {iIntInsEx.andEn,iIntInsEx.orEn,iIntInsEx.xorEn} = 'b100;
  		     'd1: {iIntInsEx.andEn,iIntInsEx.orEn,iIntInsEx.xorEn} = 'b010;
  		     'd2: {iIntInsEx.andEn,iIntInsEx.orEn,iIntInsEx.xorEn} = 'b001;
       default: {iIntInsEx.andEn,iIntInsEx.orEn,iIntInsEx.xorEn} = 'b000;
  	  endcase // bitOp
   end else if (sftSel) begin
   	  case(sftOp)
  		     'd0: {iIntInsEx.sftLeft,iIntInsEx.sftRight,iIntInsEx.sftA} = 'b100; //L
  		     'd1: {iIntInsEx.sftLeft,iIntInsEx.sftRight,iIntInsEx.sftA} = 'b010; //R
  		     'd2: {iIntInsEx.sftLeft,iIntInsEx.sftRight,iIntInsEx.sftA} = 'b011; //RA
       default: {iIntInsEx.sftLeft,iIntInsEx.sftRight,iIntInsEx.sftA} = 'b000;
  	 endcase // sftOp
   end else if (sltSel) begin
         iIntInsEx.sltEn = sltSel;
         iIntInsEx.subEn = 'b1;
   end else if (bjSel) begin
   	  case(bjOp)
  		     'd0: {iIntInsEx.addEn,iIntInsEx.subEn,iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b1_0100_0001;//jump
  		     'd1: {iIntInsEx.addEn,iIntInsEx.subEn,iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b0_1110_0010;//bge
  		     'd2: {iIntInsEx.addEn,iIntInsEx.subEn,iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b0_1110_0100;//blt
  		     'd3: {iIntInsEx.addEn,iIntInsEx.subEn,iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b0_1110_1000;//bne
  		     'd4: {iIntInsEx.addEn,iIntInsEx.subEn,iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b0_0111_0000;//beq
       default: {iIntInsEx.addEn,iIntInsEx.subEn,iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b0_0000_0000;
  	endcase // bjOp
    end else if (memSel) begin
       iIntInsEx.memEn = memSel;
       iIntInsEx.addEn = 'b1;
    end
  end

  always_comb begin
  addsubout = 'd0;
  bitout = 'd0;
  sftout = 'd0;
  tgtAddr = 'd0;
  sltout = 'd0;
  memAddr = 'd0;
   if (addsubSel) begin
    if(iIntInsEx.addEn==1)
   	    addsubout = iIntInsEx.s1+iIntInsEx.s2;
   else if (iIntInsEx.subEn==1)
        addsubout = iIntInsEx.s1-iIntInsEx.s2;
   else
   	    addsubout = 'd0;	   
   end else if (bitSel) begin
   	  case({iIntInsEx.andEn,iIntInsEx.orEn,iIntInsEx.xorEn})
  		 'b100: bitout = iIntInsEx.s1 & iIntInsEx.s2;
  		 'b010: bitout = iIntInsEx.s1 | iIntInsEx.s2;
  		 'b001: bitout = iIntInsEx.s1 ^ iIntInsEx.s2;
  	   default: bitout = 'd0;
  	endcase  
   end else if (sftSel) begin
   	 case({iIntInsEx.sftLeft,iIntInsEx.sftRight,iIntInsEx.sftA})
  		 'b100: sftout = iIntInsEx.s1 << iIntInsEx.s2[4:0];
  		 'b010: sftout = iIntInsEx.s1 >> iIntInsEx.s2[4:0];
  		 'b011: sftout = CPU_WIDTH'({{CPU_WIDTH{iIntInsEx.s1[$high(iIntInsEx.s1)]}},iIntInsEx.s1} >> iIntInsEx.s2[4:0]);
  	   default: sftout = 'd0;
  	 endcase 
   end else if (bjSel) begin
      case({iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump})
  		 'b100_0001: tgtAddr = iIntInsEx.s1 + iIntInsEx.offset;
  		 'b110_0010: tgtAddr = iIntInsEx.pc + iIntInsEx.offset; 
  		 'b110_0100: tgtAddr = iIntInsEx.pc + iIntInsEx.offset; 
  		 'b110_1000: tgtAddr = iIntInsEx.pc + iIntInsEx.offset;
  		 'b111_0000: tgtAddr = iIntInsEx.pc + iIntInsEx.offset;
  	      default: tgtAddr = 'd0;
  	 endcase      
   end else if (sltSel) begin
      	     sltout = {CPU_WIDTH{LessThan}};
   end  else if (memSel) begin
   	    memAddr = iIntInsEx.s1+iIntInsEx.s2;
    end
  end

   assign signs1 = $signed(iIntInsEx.s1);
   assign signs2 = $signed(iIntInsEx.s2);
   
  always_comb begin 
    if(iIntInsEx.flags == 1)begin
      if(iIntInsEx.s1 < iIntInsEx.s2)
        LessThan = 1;
      else
        LessThan = 0;
    end else begin
      if(signs1 < signs2)
        LessThan = 1;
      else
        LessThan = 0;
    end
  end

  always_comb begin 
   if (iIntInsEx.jump)
    iIntInsEx.linkOffset = linkOffset;
   else  
    iIntInsEx.linkOffset = 'd0;
  end
  assign linkOff = {linkOffset,1'd0};
  assign linkpc = (iIntInsEx.jump) ? iIntInsEx.s2 + {29'd0,linkOffset,1'd0} : 'd0;
  assign out = addsubout | bitout | sftout | linkpc | sltout | memAddrout ;
  always_comb begin
    equal   = (iIntInsEx.s1 == iIntInsEx.s2);
    BjEnout =  iIntInsEx.jump                   
              |(iIntInsEx.beq &  equal)           
              |(iIntInsEx.bne & !equal)         
              |(iIntInsEx.blt &  LessThan)       
              |(iIntInsEx.bge & !LessThan);      
  end 
  assign  tgtAddrout = tgtAddr ;  
  assign  memAddrout = memAddr ;

  initial begin
    forever @(posedge clk) begin
      #(half_period/5);
      if ( oIntInsEx.intRslt != out) begin
        if(iIntInsEx.addSubIns & iIntInsEx.addEn) begin
          $error("add ins rslt error,%0d != %0d, s1=%0d, s2=%0d",oIntInsEx.intRslt,out,iIntInsEx.s1,iIntInsEx.s2);
          $finish;
        end else if (iIntInsEx.addSubIns & iIntInsEx.subEn) begin
          $error("sub ins rslt error,%0d != %0d, s1=%0d, s2=%0d",oIntInsEx.intRslt,out,iIntInsEx.s1,iIntInsEx.s2);
          $finish;
        end else if (iIntInsEx.andEn) begin
          $error("and rslt error,%0d != %0d, s1=%0d, s2=%0d",oIntInsEx.intRslt,out,iIntInsEx.s1,iIntInsEx.s2);
          $finish;
        end else if (iIntInsEx.orEn) begin
          $error("or rslt error,%0d != %0d, s1=%0d, s2=%0d",oIntInsEx.intRslt,out,iIntInsEx.s1,iIntInsEx.s2);
          $finish;
        end else if (iIntInsEx.xorEn) begin
          $error("xor rslt error,%0d != %0d, s1=%0d, s2=%0d",oIntInsEx.intRslt,out,iIntInsEx.s1,iIntInsEx.s2);
          $finish;
        end else if (iIntInsEx.xorEn) begin
          $error("xor rslt error,%0d != %0d, s1=%0d, s2=%0d",oIntInsEx.intRslt,out,iIntInsEx.s1,iIntInsEx.s2);
          $finish;
        end else if (iIntInsEx.sftLeft) begin
          $error("shfit left rslt error,%0d != %0d, s1=%0d, s2=%0d",oIntInsEx.intRslt,out,iIntInsEx.s1,iIntInsEx.s2);
          $finish;
        end else if (iIntInsEx.sftRight & ~iIntInsEx.sftA) begin
          $error("shfit right rslt error,%0d != %0d, s1=%0d, s2=%0d",oIntInsEx.intRslt,out,iIntInsEx.s1,iIntInsEx.s2);
          $finish;
        end else if (iIntInsEx.sftRight & iIntInsEx.sftA) begin
          $error("arithmetic shfit right rslt error,%0d != %0d, s1=%0d, s2=%0d",oIntInsEx.intRslt,out,iIntInsEx.s1,iIntInsEx.s2);
          $finish;
        end else if (iIntInsEx.jump) begin
          $error("link pc rslt error,%0d != %0d,s2=%0d, linkOffset=%0d",oIntInsEx.intRslt,out,iIntInsEx.s2,linkOff);
          $finish;
        end 
      end else if ( oIntInsEx.BjTgt != tgtAddrout) begin 
        $error(" branchjump tagter addr error, %0d != %0d",iIntInsEx.BjTgt,tgtAddrout);
        $finish;
      end else if (oIntInsEx.BjEn != BjEnout)begin
      	$error(" branchjump enable error, %0d != %0d",oIntInsEx.BjEn,BjEnout);
        $finish;
      end else if (iIntInsEx.memEn & (oIntInsEx.memAddr != out))begin
         	$error(" memory addr error, %0d != %0d",oIntInsEx.memAddr,out);
          $finish;
         end
    end 
  end

  initial begin
    $fsdbDumpfile("tb.fsdb");
    $fsdbDumpvars(0,ZionRiscvIsaLib_IntEx_RV32_tb,"+all");
    #500000 $finish;
  end 

  ZionRiscvIsaLib_IntEx #(RV64,INT_MODULE_TYPE) U_IntEx(iIntInsEx,oIntInsEx);
`Unuse_ZionRiscvIsaLib(Rvi)
endmodule

  

