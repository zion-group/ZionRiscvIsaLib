//  RV32
module ZionRiscvIsaLib_AddSubExec_RV32_tb;
`Use_ZionRiscvIsaLib(Rvi)
  parameter CPU_WIDTH = 32; 
  parameter half_period = 5;

  logic                 clk;
  logic                 OpSel;
  logic [CPU_WIDTH-1:0] addsubout;
  logic                 lessthanout;
  logic                 lessthanrslt;
  logic                 unsignedFlg;
  logic signed [CPU_WIDTH:0]   signs1;
  logic signed [CPU_WIDTH:0]   signs2;

  `RviAddSubExItf #(0)    AddSubEx();

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
  	  OpSel = {$random()}%2;
      unsignedFlg = {$random()}%2;
  	  AddSubEx.s1 = {$random()}%(CPU_WIDTH+1);
  	  AddSubEx.s2 = {$random()}%(CPU_WIDTH+1);
    end
  end 

   always_comb begin
  	case(OpSel)
  		   'd0:  AddSubEx.op = 2'b10;
  		   'd1:  AddSubEx.op = 2'b01;
     default:  AddSubEx.op = 2'b00;
  	endcase // OpSel
  end

   always_comb begin
   if(AddSubEx.op[0]==1)
   	addsubout = AddSubEx.s1+AddSubEx.s2;
   else if (AddSubEx.op[1]==1)
   	addsubout = AddSubEx.s1-AddSubEx.s2;
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
      if (AddSubEx.op[0] && AddSubEx.rslt != addsubout) begin
        $error("add rslt error,%0d != %0d",AddSubEx.rslt,addsubout);
        $finish;
      end else if (AddSubEx.op[1] && AddSubEx.rslt != addsubout) begin 
        $error("sub rslt error, %0d != %0d",AddSubEx.rslt,addsubout);
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
    $fsdbDumpvars(0,ZionRiscvIsaLib_AddSubExec_RV32_tb,"+all");
    #500 $finish;
  end 

  `RviAddSubExec (U_AddSubExec,AddSubEx);
  `RviAddSubLessThan(U_AddSubLessThan,AddSubEx,unsignedFlg,AddSubEx.rslt[$high(AddSubEx.rslt)],lessthanrslt);
`Unuse_ZionRiscvIsaLib(Rvi)
endmodule : ZionRiscvIsaLib_AddSubExec_RV32_tb

