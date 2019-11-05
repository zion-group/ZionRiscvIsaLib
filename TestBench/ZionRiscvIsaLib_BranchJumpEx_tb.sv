module ZionRiscvIsaLib_BranchJumpEx_tb;
`Use_ZionRiscvIsaLib(Rvi)
  parameter RV64 = 0;
  parameter CPU_WIDTH = 32*(RV64+1);
  parameter half_period = 5;
  `RviBjExItf #(RV64) iBjEx();
  `RviBjExItf #(RV64) oBjEx();
  logic                 clk;
  logic                 offsel;
  logic                 equal;
  logic [1:0]           linkOffset;           
  logic                 LessThan;
  logic                 BjEnrslt;
  logic                 BjEnout;
  logic [1:0]           BjEnNoLessThanrslt;
  logic [2:0]           OpSel;
  logic [CPU_WIDTH-1:0] tgtAddrout;
  logic [CPU_WIDTH-1:0] tgtAddr;
  logic signed [CPU_WIDTH  :0] signs1;
  logic signed [CPU_WIDTH  :0] signs2;
  logic [CPU_WIDTH-1:0] LinkPcout;
  logic [CPU_WIDTH-1:0] linkPc;
  logic [CPU_WIDTH-1:0] s1Fnlout;
  logic [CPU_WIDTH-1:0] s1Fnl;

  initial begin
  	clk = 0;
  	forever #10 clk = ~clk;
  end

  initial begin
      OpSel     = 0;
      //offsel    = 0;
      iBjEx.pc  = 0;
      iBjEx.s1  = 0;
      iBjEx.s2  = 0;
      iBjEx.offset = 0;
      iBjEx.unsignedFlg = 0;
  	forever @(negedge clk) begin
  	  OpSel     = {$random()}%5;
  	  offsel    = {$random()}%2;
  	  iBjEx.pc  = {$random()}%(CPU_WIDTH+1);
  	  iBjEx.s1  = {$random()}%(CPU_WIDTH+1);
  	  iBjEx.s2  = {$random()}%(CPU_WIDTH+1);
  	  iBjEx.offset = {$random()}%(CPU_WIDTH+1);
  	  iBjEx.unsignedFlg = {$random()}%2; 
    end
  end

  always_comb begin
  	case(OpSel)
  		 'd0:  {iBjEx.bjEn,iBjEx.branch,iBjEx.beq,iBjEx.bne,iBjEx.blt,iBjEx.bge,iBjEx.jump} = 'b100_0001;//jump
  		 'd1:  {iBjEx.bjEn,iBjEx.branch,iBjEx.beq,iBjEx.bne,iBjEx.blt,iBjEx.bge,iBjEx.jump} = 'b110_0010;//bje
  		 'd2:  {iBjEx.bjEn,iBjEx.branch,iBjEx.beq,iBjEx.bne,iBjEx.blt,iBjEx.bge,iBjEx.jump} = 'b110_0100;//blt
  		 'd3:  {iBjEx.bjEn,iBjEx.branch,iBjEx.beq,iBjEx.bne,iBjEx.blt,iBjEx.bge,iBjEx.jump} = 'b110_1000;//bne
  		 'd4:  {iBjEx.bjEn,iBjEx.branch,iBjEx.beq,iBjEx.bne,iBjEx.blt,iBjEx.bge,iBjEx.jump} = 'b111_0000;//beq
     default:{iBjEx.bjEn,iBjEx.branch,iBjEx.beq,iBjEx.bne,iBjEx.blt,iBjEx.bge,iBjEx.jump} = 'b000_0000;
  	endcase // OpSel
  end


  always_comb begin
  	case(offsel)
  		 'd0:   linkOffset = 2'b01;  
  		 'd1:   linkOffset = 2'b10;
     //full_case
  	endcase // offsel
  end

  always_comb begin 
   if (iBjEx.jump)
    iBjEx.linkOffset = linkOffset;
   else  
    iBjEx.linkOffset = 'd0;
  end

  always_comb begin
  	case({iBjEx.bjEn,iBjEx.branch,iBjEx.beq,iBjEx.bne,iBjEx.blt,iBjEx.bge,iBjEx.jump})
  		 'b100_0001: tgtAddrout = iBjEx.s1 + iBjEx.offset;
  		 'b110_0010: tgtAddrout = iBjEx.pc + iBjEx.offset; 
  		 'b110_0100: tgtAddrout = iBjEx.pc + iBjEx.offset; 
  		 'b110_1000: tgtAddrout = iBjEx.pc + iBjEx.offset;
  		 'b111_0000: tgtAddrout = iBjEx.pc + iBjEx.offset;
  	      default: tgtAddrout = 'd0;
  	endcase 
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
  	equal    = (iBjEx.s1 == iBjEx.s2);
    BjEnout  = iBjEx.jump 
              |(iBjEx.beq & equal)
              |(iBjEx.bne & !equal) 
              |(iBjEx.blt &  LessThan)       
              |(iBjEx.bge & !LessThan);      
  end

  assign LinkPcout = (iBjEx.jump) ? iBjEx.pc + {linkOffset,1'b0} : 'd0;
  
  assign s1Fnlout  = (iBjEx.jump) ? {29'd0,linkOffset,1'b0} : iBjEx.s1 ;
   
 initial begin
   forever @(posedge clk) begin
      #(half_period/5);
      if (tgtAddr != tgtAddrout)begin
        $error("tgtAddr rslt error, %0d != %0d",tgtAddr,tgtAddrout);
        $finish;
      end
    end 
 end

 initial begin
   forever @(posedge clk) begin
      #(half_period/5);
      if (BjEnrslt != BjEnout)begin
        $error("BjEnNoLessThan rslt error, %0d != %0d",BjEnNoLessThanrslt,BjEnout);
        $finish;
      end
    end 
 end

 initial begin
   forever @(posedge clk) begin
      #(half_period/5);
      if (BjEnrslt != BjEnout)begin
        $error("BjEn rslt error, %0d != %0d",BjEnrslt,BjEnout);
        $finish;
      end
    end 
 end

 initial begin
   forever @(posedge clk) begin
      #(half_period/5);
      if (linkPc != LinkPcout)begin
        $error("LinkPc rslt error, %0d != %0d",linkPc,LinkPcout);
        $finish;
      end
    end 
 end

 initial begin
    forever @(posedge clk) begin
       #(half_period/5);
       if (oBjEx.s1Fnl != s1Fnlout)begin
         $error("s1Fnl rslt error, %0d != %0d",oBjEx.s1Fnl,s1Fnlout);
         $finish;
       end
     end 
  end

 initial begin
   $fsdbDumpfile("tb.fsdb");
   $fsdbDumpvars(0,ZionRiscvIsaLib_BranchJumpEx_tb,"+all");
   #50000 $finish;
 end 

 `RviBjTgtAddr (U_BjTgtAddr,iBjEx,tgtAddr); 
 `RviBjEnNoLessThan (U_BjEnNoLessThan,iBjEx,LessThan,BjEnNoLessThanrslt);
 `RviBjEnGen (U_BjEnGen,iBjEx,BjEnrslt);
 `RviJumpLinkPc (U_JumpLinkPc,iBjEx,linkPc);
 `RviS1LinkOffsetMux (U_S1LinkOffsetMux,iBjEx,s1Fnl);

`Unuse_ZionRiscvIsaLib(Rvi)
endmodule : ZionRiscvIsaLib_BranchJumpEx_tb









