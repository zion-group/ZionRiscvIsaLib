module ZionRiscvIsaLib_SftEx_RV32_tb;
`Use_ZionRiscvIsaLib(Rvi)
  parameter RV64=0;
  parameter CPU_WIDTH = 32*(RV64+1); 
  parameter half_period = 5;
  `RviSftExItf #(RV64) iSftEx();
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
      iSftEx.s1 = 0;
      iSftEx.s2 = 0;
  	forever @(negedge clk) begin
  	  OpSel = {$random()}%3;
  	  iSftEx.s1 = {$random()}%(CPU_WIDTH+1);
  	  iSftEx.s2 = {$random()}%(4+RV64+2);
    end
  end 

  always_comb begin
  	case(OpSel)
  		 'd0:  {iSftEx.op[0],iSftEx.op[1],iSftEx.op[2]} = 'b100; //L
  		 'd1:  {iSftEx.op[0],iSftEx.op[1],iSftEx.op[2]} = 'b010; //R
  		 'd2:  {iSftEx.op[0],iSftEx.op[1],iSftEx.op[2]} = 'b011; //RA
    default: {iSftEx.op[0],iSftEx.op[1],iSftEx.op[2]} = 'b000;
  	endcase // OpSel
  end

   always_comb begin
  	case({iSftEx.op[0],iSftEx.op[1],iSftEx.op[2]})
  		 'b100: out = iSftEx.s1 << iSftEx.s2;
  		 'b010: out = iSftEx.s1 >> iSftEx.s2;
  		 'b011: out = CPU_WIDTH'({{CPU_WIDTH{iSftEx.s1[$high(iSftEx.s1)]}},iSftEx.s1} >> iSftEx.s2);
  	default:  out = 'd0;
  	endcase 
  end

  initial begin
    forever @(posedge clk) begin
      #(half_period/5);
      if (iSftEx.op[0] && rslt != out) begin
        $error("shift left rslt error,%0d != %0d",rslt,out);
        $finish;
      end else if (iSftEx.op[2] == 0 && iSftEx.op[1] && rslt != out) begin 
        $error("shift right rslt error, %0d != %0d",rslt,out);
        $finish;
      end else if (iSftEx.op[2] && iSftEx.op[1] && rslt != out) begin 
        $error("arithmetic shift rslt error,%0d != %0d",rslt,out);
        $finish;
      end
    end 
  end 

  initial begin
    $fsdbDumpfile("tb.fsdb");
    $fsdbDumpvars(0,ZionRiscvIsaLib_SftEx_RV32_tb,"+all");
    #500 $finish;
  end 
 
  `RviSftExec (U_SftEx,iSftEx,rslt);
`Unuse_ZionRiscvIsaLib(Rvi) 
endmodule : ZionRiscvIsaLib_SftEx_RV32_tb