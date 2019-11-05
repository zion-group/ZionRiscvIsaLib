module refdef(
	output logic            oBjEn
	);
  parameter RV64=0;
  parameter CPU_WIDTH = 32*(RV64+1); 
 logic [CPU_WIDTH-1:0]LinkPc;
 ZionRiscvIsaLib_BjExItf BjExItf();
	`ZionRiscvIsaLib_BjEnGen(U_BjEnGen,BjExItf,oBjEn);
	`ZionRiscvIsaLib_JumpLinkPc(U_JumpLinkPc,BjExItf,LinkPc);
endmodule : refdef