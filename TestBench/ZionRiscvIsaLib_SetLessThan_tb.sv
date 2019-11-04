module ZionRiscvIsaLib_SetLessThan_tb;
`Use_ZionRiscvIsaLib(Rvi)
  parameter RV64=0;
  parameter CPU_WIDTH = 32*(RV64+1);  
  parameter half_period = 5;
  `RviSltExItf #(RV64) iSltEx();
  logic                 clk;
  logic                 out;
  logic [CPU_WIDTH-1:0] rslt;
  initial begin
    clk = 0;
    forever #10 clk = ~clk;
  end
  initial begin
    iSltEx.s1 = 0;
    iSltEx.s2 = 0;
    iSltEx.en = 0;
    iSltEx.unsignedFlg = 0;
    forever @(negedge clk) begin
      iSltEx.s1 = $random()%(CPU_WIDTH+1);
      iSltEx.s2 = $random()%(CPU_WIDTH+1);
      iSltEx.unsignedFlg = {$random()}%2;
      iSltEx.en = {$random()}%2;
    end
  end 

  always_comb begin
    if (iSltEx.en == 1) begin
      if (((~iSltEx.unsignedFlg) & iSltEx.s1[CPU_WIDTH-1]) == ((~iSltEx.unsignedFlg) & iSltEx.s2[CPU_WIDTH-1])) begin
       if ((iSltEx.s1 > iSltEx.s2)||(iSltEx.s1 == iSltEx.s2)) begin
       out = 1'b0;
       end else if (iSltEx.s1 < iSltEx.s2) begin
       out = 1'b1;
       end
       end if ((((~iSltEx.unsignedFlg) & iSltEx.s1[CPU_WIDTH-1]) == 0) && (((~iSltEx.unsignedFlg) & iSltEx.s2[CPU_WIDTH-1]) == 1)) begin
       out = 1'b0;
       end else if ((((~iSltEx.unsignedFlg) & iSltEx.s1[CPU_WIDTH-1]) == 1) && (((~iSltEx.unsignedFlg) & iSltEx.s2[CPU_WIDTH-1]) == 0)) begin
       out = 1'b1;
       end 
     end else if (iSltEx.en == 0) begin
     out = 1'b0;
   end
 end


 initial begin
      forever@(posedge clk) begin
      #(half_period/5);
        if (iSltEx.en == 1) begin
          if (~iSltEx.unsignedFlg == 1) begin
            if (iSltEx.s1[CPU_WIDTH-1] > iSltEx.s2[CPU_WIDTH-1]) begin
              if (out != rslt) begin
                $error("s1 < s2,%d < %d,s1 Ne,s2 Po",iSltEx.s1,iSltEx.s2);
                $finish;
              end 
            end else if (iSltEx.s1[CPU_WIDTH-1] < iSltEx.s2[CPU_WIDTH-1]) begin
            if (out != rslt) begin
              $error("s1 > s2,%d > %d,s1 Po,s2 Ne",iSltEx.s1,iSltEx.s2);
              $finish;
            end 
          end else if ((iSltEx.s1[CPU_WIDTH-1] == 0) && (iSltEx.s2[CPU_WIDTH-1] == 0)) begin
          if (iSltEx.s1 > iSltEx.s2) begin
            if (out != rslt) begin
              $error("s2 < s1,%d < %d,s1 Po,s2 Po",iSltEx.s2,iSltEx.s1);
              $finish;
            end
          end else if (iSltEx.s1 < iSltEx.s2) begin
          if (out != rslt) begin
              $error("s2 > s1,%d > %d,s1 Po,s2 Po",iSltEx.s2,iSltEx.s1);
              $finish;
            end
          end 
        end else if ((iSltEx.s1[CPU_WIDTH-1] == 1) && (iSltEx.s2[CPU_WIDTH-1] == 1)) begin
        if (iSltEx.s1 > iSltEx.s2) begin
          if (out != rslt) begin
            $error("s2 < s1,%d > %d,s1 Ne,s2 Ne",iSltEx.s2,iSltEx.s1);
            $finish;
          end 
        end else if (iSltEx.s1 < iSltEx.s2) begin
        if (out != rslt) begin
          $error("s2 > s1,%d < %d,s1 Ne,s2 Ne",iSltEx.s2,iSltEx.s1);
          $finish;
        end
      end
    end
  end else if (~iSltEx.unsignedFlg == 0) begin
  if (iSltEx.s1 > iSltEx.s2) begin
    if (out != rslt) begin
      $error("s2 < s1,%d < %d,s1,s2 unsign",iSltEx.s2,iSltEx.s1);
      $finish;
    end
  end else if (iSltEx.s1 < iSltEx.s2) begin
  if (out != rslt) begin
    $error("s2 > s1,%d > %d,s1,s2 unsign",iSltEx.s2,iSltEx.s1);
    $finish;
  end
end
end 
end else if (iSltEx.en == 0) begin
  if (out != 0 || rslt != 0) begin
    $error("en error");
    $finish;
  end
end
end
end

  initial begin
    $fsdbDumpfile("tb.fsdb");
    $fsdbDumpvars(0,ZionRiscvIsaLib_SetLessThan_tb,"+all");
    #500 $finish;
  end 
 
  `RviSetLessThan (U_SltEx,iSltEx,rslt);
`Unuse_ZionRiscvIsaLib(Rvi) 
endmodule : ZionRiscvIsaLib_SetLessThan_tb