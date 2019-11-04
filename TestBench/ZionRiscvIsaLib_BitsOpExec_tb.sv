module ZionRiscvIsaLib_BitsOpExec_tb;
`Use_ZionRiscvIsaLib(Rvi)
  parameter RV64=0;
  parameter CPU_WIDTH = 32*(RV64+1); 
  parameter half_period = 5;
  `RviBitsExItf #(RV64) iBitsEx();
  logic                 clk;
  logic [1:0]           OpSel;
  logic [CPU_WIDTH-1:0] out;
  logic [CPU_WIDTH-1:0] rslt;
 
  initial begin
  	clk = 0;
  	forever #10 clk = ~clk;
  end

  initial begin
      OpSel = 0;
      iBitsEx.s1 = 0;
      iBitsEx.s2 = 0;
  	forever @(negedge clk) begin
  	  OpSel = {$random()}%3;
  	  iBitsEx.s1 = {$random()}%(CPU_WIDTH+1);
  	  iBitsEx.s2 = {$random()}%(CPU_WIDTH+1);
    end
  end 
  
  always_comb begin
  	case(OpSel)
  		 'd0:  {iBitsEx.andEn,iBitsEx.orEn,iBitsEx.xorEn} = 'b100;
  		 'd1:  {iBitsEx.andEn,iBitsEx.orEn,iBitsEx.xorEn} = 'b010;
  		 'd2:  {iBitsEx.andEn,iBitsEx.orEn,iBitsEx.xorEn} = 'b001;
   default:  {iBitsEx.andEn,iBitsEx.orEn,iBitsEx.xorEn} = 'b000;
  	endcase // OpSel
  end

   always_comb begin
  	case({iBitsEx.andEn,iBitsEx.orEn,iBitsEx.xorEn})
  		 'b100: out = iBitsEx.s1 & iBitsEx.s2;
  		 'b010: out = iBitsEx.s1 | iBitsEx.s2;
  		 'b001: out = iBitsEx.s1 ^ iBitsEx.s2;
  	 default: out = 'd0;
  	endcase 
  end
  
  // initial begin
  // 	forever @(posedge clk) begin
  // 		if (oRslt != out) begin
  // 			$error("oRslt error,%0d != %0d",oRslt,out);
  // 			$finish;
  // 		end
  // 	end 
  // end 

  initial begin
    forever @(posedge clk) begin
      #(half_period/5);
      if (iBitsEx.andEn && rslt != out) begin
        $error("and rslt error,%0d != %0d",rslt,out);
        $finish;
      end else if (iBitsEx.orEn && rslt != out) begin 
        $error("or rslt error, %0d != %0d",rslt,out);
        $finish;
      end else if (iBitsEx.xorEn && rslt != out) begin 
        $error("xor rslt error,%0d != %0d",rslt,out);
        $finish;
      end
    end 
  end 

  initial begin
    $fsdbDumpfile("tb.fsdb");
    $fsdbDumpvars(0,ZionRiscvIsaLib_BitsOpExec_tb,"+all");
    #500 $finish;
  end 
 
  `RviBitsOpExec (U_BitsEx,iBitsEx,rslt);
`Unuse_ZionRiscvIsaLib(Rvi) 
endmodule : ZionRiscvIsaLib_BitsOpExec_tb
