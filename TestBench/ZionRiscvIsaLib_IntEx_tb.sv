module ZionRiscvIsaLib_IntEx_RV32_tb;
`Use_ZionRiscvIsaLib(Rvi)
  parameter RV64=0;
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
  logic                        sltout；
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



  initial begin
    clk = 0;
    forever #10 clk = ~clk;
  end
  initial begin
  	OpSel = 0;
  	addsubOp = 0;
  	bitOp = 0;
    iIntInsEx.s1 = 0;
    iIntInsEx.s2 = 0;
    iIntInsEx.pc = 0;
    iIntInsEx.flags  = 0;
    iIntInsEx.offset = 0;

    forever @(negedge clk) begin
      OpSel           = {$random()}%7;
      addsubOp        = {$random()}%2;
      bitOp           = {$random()}%3;
      iIntInsEx.flags = {$random()}%2;
      iIntInsEx.s1 =  $random()%(CPU_WIDTH+1);
      iIntInsEx.s2 =  $random()%(CPU_WIDTH+1);
      iIntInsEx.pc = {$random()}%(CPU_WIDTH+1);
      iIntInsEx.offset = {$random()}%(CPU_WIDTH+1);      
    end
  end 

  always_comb begin
  	case(OpSel)
  		 'd0:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b100000;
  		 'd1:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b010000; 
  		 'd2:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b001000;
  		 'd3:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b000100;
  		 'd4:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b000010;
  		 'd5:  {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b000001; 
    default: {addsubSel,bitSel,sftSel,sltSel,bjSel,memSel} = 'b000000;
  	endcase // OpSel
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
   end else if (bjSel) begin
   	  case(bjOp)
  		     'd0: {iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b100_0001;//jump
  		     'd1: {iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b110_0010;//bje
  		     'd2: {iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b110_0100;//blt
  		     'd3: {iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b110_1000;//bne
  		     'd4: {iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b111_0000;//beq
         default: {iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump} = 'b000_0000;
  	endcase // bjOp
    end// else if (memSel) begin
   // 	    iIntInsEx.memEn = memSel;
   // end
  end

  always_comb begin
  	{iIntInsEx.addEn,     iIntInsEx.subEn,    iIntInsEx.andEn, iIntInsEx.orEn,  iIntInsEx.xorEn} = 'd0;
    {iIntInsEx.sftLeft,   iIntInsEx.sftRight, iIntInsEx.sftA,  iIntInsEx.sltEn, iIntInsEx.memEn} = 'd0;
    {iIntInsEx.addSubIns, iIntInsEx.bjEn,     iIntInsEx.branch,iIntInsEx.beq,   iIntInsEx.bne, iIntInsEx.blt, iIntInsEx.bge, iIntInsEx.jump} = 'd0;
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
  		 'b100: sftout = iIntInsEx.s1 << iIntInsEx.s2;
  		 'b010: sftout = iIntInsEx.s1 >> iIntInsEx.s2;
  		 'b011: sftout = CPU_WIDTH'({{CPU_WIDTH{iIntInsEx.s1[$high(iIntInsEx.s1)]}},iIntInsEx.s1} >> iIntInsEx.s2);
  	   default: sftout = 'd0;
  	 endcase 
   end else if (bjSel) begin
      case({iIntInsEx.bjEn,iIntInsEx.branch,iIntInsEx.beq,iIntInsEx.bne,iIntInsEx.blt,iIntInsEx.bge,iIntInsEx.jump})
  		 'b100_0001: tgtAddrout = iIntInsEx.s1 + iIntInsEx.offset;
  		 'b110_0010: tgtAddrout = iIntInsEx.pc + iIntInsEx.offset; 
  		 'b110_0100: tgtAddrout = iIntInsEx.pc + iIntInsEx.offset; 
  		 'b110_1000: tgtAddrout = iIntInsEx.pc + iIntInsEx.offset;
  		 'b111_0000: tgtAddrout = iIntInsEx.pc + iIntInsEx.offset;
  	        default: tgtAddrout = 'd0;
  	 endcase      
    end else if (sltSel) begin
      	     sltout = LessThan;
     end // else if (memSel) begin
   	    
   // end
  end

   assign signs1 = $signed(iBjEx.s1);
   assign signs2 = $signed(iBjEx.s2);

  always_comb begin 
    if(iBjEx.unsignedFlg == 1)begin
      if(iBjEx.s1 < iBjEx.s2)
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
  	equal      = (iIntInsEx.s1 == iIntInsEx.s2);
  	BjEnout[1] =  iIntInsEx.jump                   
              |(iIntInsEx.beq &  equal)           
              |(iIntInsEx.bne & !equal);         
    BjEnout[0] = (iIntInsEx.blt &  LessThan)       
              |(iIntInsEx.bge & !LessThan);      
  end

  assign linkpc = (iIntInsEx.jump) ? iIntInsEx.pc +'d4 : 'd0;
  assign out = addsubout | bitout | sftout | tgtAddrout | linkpc | sltout;  

  initial begin
    forever @(posedge clk) begin
      #(half_period/5);
      if ( oIntInsEx.intRslt != out) begin
        $error("int rslt error,%0d != %0d",oIntInsEx.intRslt,out);
        $finish;
      end else if ( oIntInsEx.BjTgt != tgtAddrout) begin 
        $error(" branchjump tagter addr error, %0d != %0d",iIntInsEx.rslt,tgtAddrout);
        $finish;
      end else if (oIntInsEx.BjEn != BjEnout)begin
      	$error(" branchjump enable error, %0d != %0d",oIntInsEx.BjEn,BjEnout);
        $finish;
      end //else if (oIntInsEx.memaddr != )begin
      // 	$error(" branchjump enable error, %0d != %0d",oIntInsEx.BjEn,BjEnout);
      //   $finish;
      // end
    end 
  end

  ZionRiscvIsaLib_IntEx #(0,0) U_IntEx(iIntInsEx,oIntInsEx);
`Unuse_ZionRiscvIsaLib(Rvi)
endmodule

  

