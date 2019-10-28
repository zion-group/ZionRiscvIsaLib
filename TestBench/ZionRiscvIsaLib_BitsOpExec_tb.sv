module ZionRiscvIsaLib_BitsOpExec_tb;
`Use_ZionRiscvIsaLib(Ri)
  parameter RV64=0;
  parameter CPU_WIDTH = 32*(RV64+1); 
  parameter half_period = 5;
  `RiBitsExItf #(RV64) iBitsEx();
  logic                 clk;
  logic [1:0]           OpSel;
  logic [CPU_WIDTH-1:0] out;
 
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
      if (iBitsEx.andEn && iBitsEx.rslt != out) begin
        $error("and rslt error,%0d != %0d",iBitsEx.rslt,out);
        $finish;
      end else if (iBitsEx.orEn && iBitsEx.rslt != out) begin 
        $error("or rslt error, %0d != %0d",iBitsEx.rslt,out);
        $finish;
      end else if (iBitsEx.xorEn && iBitsEx.rslt != out) begin 
        $error("xor rslt error,%0d != %0d",iBitsEx.rslt,out);
        $finish;
      end
    end 
  end 

  initial begin
    $fsdbDumpfile("tb.fsdb");
    $fsdbDumpvars(0,ZionRiscvIsaLib_BitsOpExec_tb,"+all");
    #500 $finish;
  end 
 
  `RiBitsOpExec (U_BitsEx,iBitsEx);
`Unuse_ZionRiscvIsaLib(Ri) 
endmodule : ZionRiscvIsaLib_BitsOpExec_tb
