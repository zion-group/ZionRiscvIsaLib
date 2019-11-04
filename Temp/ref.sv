module ref(
	ZionRiscvIsaLib_BjExItf BjExItf,
	output logic    [2:0] oBjEn
	);
	`ZionRiscvIsaLib_BjEnGen(U_BjEnGen,BjExItf,oBjEn);
	`ZionRiscvIsaLib_JumpLinkPc(U_JumpLinkPc,BjExItf,BjExItf.JumpLinkPcOut);
endmodule : ref