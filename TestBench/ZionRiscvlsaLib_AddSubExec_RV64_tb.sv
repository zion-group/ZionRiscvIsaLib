//  RV64
module ZionRiscvlsaLib_AddSubExec_RV64_tb;
`Use_ZionRiscvIsaLib(Rvi)
  parameter CPU_WIDTH = 64; 
  parameter half_period = 5;

  logic                 clk;
  logic [1:0]           OpSel;
  logic [CPU_WIDTH-1:0] addsubout;
  logic [CPU_WIDTH-1:0] outtmp;
  logic [CPU_WIDTH-1:0] rslt;
  logic                 lessthanout;
  logic                 lessthanrslt;
  logic                 unsignedFlg;
  logic signed [CPU_WIDTH:0]   signs1;
  logic signed [CPU_WIDTH:0]   signs2;

  `RviAddSubExItf #(1)    AddSubEx();

  initial begin
  	clk = 0;
  	forever #10 clk = ~clk;
  end

  initial begin
      OpSel = 0;
      AddSubEx.s1 = 0;
      AddSubEx.s2 = 0;
      unsignedFlg = 0;
  	forever @(negedge clk) begin
  	  OpSel = {$random()}%4;
      unsignedFlg = {$random()}%2;
  	  AddSubEx.s1 = {$random()}%(CPU_WIDTH+1);
  	  AddSubEx.s2 = {$random()}%(CPU_WIDTH+1);
    end
  end 

   always_comb begin
  	case(OpSel)
  	  'd0:  AddSubEx.op = 3'b010;
  	  'd1:  AddSubEx.op = 3'b001;
	    'd2:  AddSubEx.op = 3'b110;
	    'd3:  AddSubEx.op = 3'b101;
      default:  AddSubEx.op = 3'b000;
  	endcase // OpSel
  end

   always_comb begin
    if(AddSubEx.op[0]==1)
   	 outtmp = AddSubEx.s1+AddSubEx.s2;
    else if (AddSubEx.op[1]==1)
   	 outtmp = AddSubEx.s1-AddSubEx.s2;
    else
   	 outtmp = 'd0;
   end

   always_comb begin
    if(AddSubEx.op[2]==1)
	    addsubout = {{32{outtmp[31]}},outtmp[31:0]};
    else if(AddSubEx.op[2]==0)
	    addsubout = outtmp;
	  else 
      addsubout = 'd0;	
   end
   
   assign signs1 = $signed(AddSubEx.s1);
   assign signs2 = $signed(AddSubEx.s2);

   always_comb begin 
    if(unsignedFlg == 1)begin
      if(AddSubEx.s1 < AddSubEx.s2)
        lessthanout = 1;
      else
        lessthanout = 0;
    end else begin
      if(signs1 < signs2)
        lessthanout = 1;
      else
        lessthanout = 0;
    end
   end

  initial begin
    forever @(posedge clk) begin
      #(half_period/5);
      if (AddSubEx.op[0] && rslt != addsubout) begin
        $error("add rslt error,%0d != %0d",rslt,addsubout);
        $finish;
      end else if (AddSubEx.op[1] && rslt != addsubout) begin 
        $error("sub rslt error, %0d != %0d",rslt,addsubout);
        $finish;
      end
    end 
  end

 initial begin
   forever @(posedge clk) begin
      #(half_period/5);
      if (AddSubEx.op[1] == 1 && lessthanrslt != lessthanout)begin
        $error("lessthan rslt error, %0d != %0d",lessthanrslt,lessthanout);
        $finish;
      end
    end 
 end 

  initial begin
    $fsdbDumpfile("tb.fsdb");
    $fsdbDumpvars(0,ZionRiscvlsaLib_AddSubExec_RV64_tb,"+all");
    #500 $finish;
  end 

  `RviAddSubExec (U_AddSubExec,AddSubEx,rslt);
  `RviAddSubLessThan(U_AddSubLessThan,AddSubEx,unsignedFlg,AddSubEx.rslt[$high(AddSubEx.rslt)],lessthanrslt);
`Unuse_ZionRiscvIsaLib(Rvi)
endmodule : ZionRiscvlsaLib_AddSubExec_RV64_tb