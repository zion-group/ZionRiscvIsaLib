`ifndef Disable_ZionRiscvIsaLib_LoadExItf
interface ZionRiscvIsaLib_LoadExItf
#(RV64 = 0);

  localparam CPU_WIDTH = 32*(RV64+1);
  logic [(2+RV64):0] loadEn;
  logic              unsignedLoad;
  logic [(1+RV64):0] addr;
  logic [CPU_WIDTH-1:0] memDat,LoadRslt;

  function automatic logic [CPU_WIDTH-1:0] Exec;
    
    logic [CPU_WIDTH-1:0] result;
    // LB 
    logic [CPU_WIDTH/8-1:0][7:0] byteSplitDat;
    logic                  [7:0] byteLoadDat;
    logic                        byteMsb,byteExtend;
    logic [CPU_WIDTH-1  :0]      byteLoadRslt;

    logic [CPU_WIDTH/16-1:0][15:0] halfwordSplitDat;
    logic                   [15:0] halfwordLoadDat;
    logic                          halfwordMsb,halfwordExtend;
    logic [CPU_WIDTH-1   :0]       halfwordLoadRslt;

    logic [CPU_WIDTH-1   :0] wordLoadRslt;
    logic [CPU_WIDTH/32-1:0][31:0] wordSplitDat;
    logic                   [31:0] wordLoadDat;
    logic                          wordMsb,wordExtend ;
   
    logic [CPU_WIDTH-1   :0] doubleLoadRslt;
    // for (int i=0;i<CPU_WIDTH/8;i++)begin
    //   byteSplitDat[i] = memDat[i*8+:8];
    // end
    byteSplitDat = memDat;
    byteLoadDat  = byteSplitDat[addr];//memDat[7:0];//addr[$high(addr)//[$high(addr)]
    byteExtend   = (~unsignedLoad) & byteLoadDat[$high(byteLoadDat)];
    byteLoadRslt = {CPU_WIDTH{loadEn[0]}} & {{(CPU_WIDTH-$bits(byteLoadDat)){byteExtend}}, byteLoadDat};
    // LH
    
    halfwordSplitDat = memDat;
    halfwordLoadDat  = halfwordSplitDat[addr[$high(addr)]];//addr[$high(addr)
    halfwordExtend   = (~unsignedLoad) & halfwordLoadDat[$high(halfwordLoadDat)];
    halfwordLoadRslt = {CPU_WIDTH{loadEn[1]}} & {{(CPU_WIDTH-$bits(halfwordLoadDat)){halfwordExtend}}, halfwordLoadDat};
    `gen_if(RV64==0)begin
      // LW
      $display("======");
      wordLoadRslt = {CPU_WIDTH{loadEn[2]}} & memDat;
      result = byteLoadRslt | halfwordLoadRslt | wordLoadRslt;
    end `gen_else begin
      // LW
      $display("********");
      wordSplitDat = memDat;
      wordLoadDat  = wordSplitDat[addr[$high(addr)]];//addr[$high(addr)
      wordExtend   = (~unsignedLoad) & wordLoadDat[$high(wordLoadDat)];
      wordLoadRslt = {CPU_WIDTH{loadEn[2]}} & {{(CPU_WIDTH-$bits(wordLoadDat)){wordExtend}}, wordLoadDat};
      //LD
      
      doubleLoadRslt = {CPU_WIDTH{loadEn[3]}} & memDat;
      result = byteLoadRslt | halfwordLoadRslt | wordLoadRslt | doubleLoadRslt;
    end
    return result;

  endfunction: Exec

  modport De (output loadEn, unsignedLoad, addr, memDat);
  modport Ex (input  loadEn, unsignedLoad, addr, memDat, import Exec);

  always_comb begin 
   LoadRslt     = Exec();
  end
endinterface: ZionRiscvIsaLib_LoadExItf
`endif

