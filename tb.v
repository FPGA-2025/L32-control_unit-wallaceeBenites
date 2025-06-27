`timescale 1ns/1ps

module tb();


reg clk = 0;
reg rst_n;
reg [6:0] instruction_opcode;
wire pc_write;
wire ir_write;
wire pc_source;
wire reg_write;
wire memory_read;
wire is_immediate;
wire memory_write;
wire pc_write_cond;
wire lorD;
wire memory_to_reg;
wire [1:0] aluop;
wire [1:0] alu_src_a;
wire [1:0] alu_src_b;

reg expected_pc_write;
reg expected_ir_write;
reg expected_pc_source;
reg expected_reg_write;
reg expected_memory_read;
reg expected_is_immediate;
reg expected_memory_write;
reg expected_pc_write_cond;
reg expected_lorD;
reg expected_memory_to_reg;
reg [1:0] expected_aluop;
reg [1:0] expected_alu_src_a;
reg [1:0] expected_alu_src_b;

reg [22:0] file_data [0:4];
// 0110011_0_0_0_1_0_0_0_0_0_0_00_00_00 7+10+2+2+2 = 23 bits

// Instruction Opcodes
localparam LW      = 7'b0000011;
localparam SW      = 7'b0100011;
localparam RTYPE   = 7'b0110011;
localparam ITYPE   = 7'b0010011;
localparam JALI    = 7'b1101111;
localparam BRANCHI = 7'b1100011;
localparam JALRI   = 7'b1100111;
localparam AUIPCI  = 7'b0010111;
localparam LUII    = 7'b0110111;
localparam CSR     = 7'b1110011;

Control_Unit c_un(
    .clk(clk),
    .rst_n(rst_n),
    .instruction_opcode(instruction_opcode),
    .pc_write(pc_write),
    .ir_write(ir_write),
    .pc_source(pc_source),
    .reg_write(reg_write),
    .memory_read(memory_read),
    .is_immediate(is_immediate),
    .memory_write(memory_write),
    .pc_write_cond(pc_write_cond),
    .lorD(lorD),
    .memory_to_reg(memory_to_reg),
    .aluop(aluop),
    .alu_src_a(alu_src_a),
    .alu_src_b(alu_src_b)
);

always #1 clk = ~clk;

integer i;
integer j;

initial begin
    $dumpfile("saida.vcd");
    $dumpvars(0, tb);
    $readmemb("teste.txt", file_data);
    
    instruction_opcode = file_data[0][22:16];

    case (instruction_opcode)
        LW      : j=5;
        SW      : j=4;
        RTYPE   : j=4;
        ITYPE   : j=4;
        JALI    : j=4;
        BRANCHI : j=3;
        JALRI   : j=5;
        AUIPCI  : j=4;
        LUII    : j=4;
        default : j=0;
    endcase

    rst_n = 1'b0;
    #4
    rst_n = 1'b1;
    
    for (i=0; i<j; i++) begin

        expected_pc_write       = file_data[i][15];
        expected_ir_write       = file_data[i][14];
        expected_pc_source      = file_data[i][13];
        expected_reg_write      = file_data[i][12];
        expected_memory_read    = file_data[i][11];
        expected_is_immediate   = file_data[i][10];
        expected_memory_write   = file_data[i][9];
        expected_pc_write_cond  = file_data[i][8];
        expected_lorD           = file_data[i][7];
        expected_memory_to_reg  = file_data[i][6];
        expected_aluop          = file_data[i][5:4];
        expected_alu_src_a      = file_data[i][3:2];
        expected_alu_src_b      = file_data[i][1:0];

        if (pc_write        === expected_pc_write       &&
            ir_write        === expected_ir_write       &&
            pc_source       === expected_pc_source      &&
            reg_write       === expected_reg_write      &&
            memory_read     === expected_memory_read    &&
            is_immediate    === expected_is_immediate   &&
            memory_write    === expected_memory_write   &&
            pc_write_cond   === expected_pc_write_cond  &&
            lorD            === expected_lorD           &&
            memory_to_reg   === expected_memory_to_reg  &&
            aluop           === expected_aluop          &&
            alu_src_a       === expected_alu_src_a      &&
            alu_src_b       === expected_alu_src_b ) begin
                $display("=== OK ",
                    "Ciclo na FSM = ", i,
                    "\nopcode = ", instruction_opcode, 
                    " pc_write = ", pc_write, 
                    " ir_write = ", ir_write, 
                    " pc_source = ", pc_source, 
                    " reg_write = ", reg_write, 
                    " memory_read = ", memory_read, 
                    " is_immediate = ", is_immediate, 
                    " memory_write = ", memory_write, 
                    " pc_write_cond = ", pc_write_cond, 
                    " lorD = ", lorD, 
                    " memory_to_reg = ", memory_to_reg, 
                    " aluop = ", aluop, 
                    " alu_src_a = ", alu_src_a, 
                    " alu_src_b = ", alu_src_b);
            end else begin
                $display("=== ERRO ",
                    "Ciclo na FSM = ", i,
                    "\nopcode = ", instruction_opcode, 
                    " pc_write = ", pc_write, 
                    " ir_write = ", ir_write, 
                    " pc_source = ", pc_source, 
                    " reg_write = ", reg_write, 
                    " memory_read = ", memory_read, 
                    " is_immediate = ", is_immediate, 
                    " memory_write = ", memory_write, 
                    " pc_write_cond = ", pc_write_cond, 
                    " lorD = ", lorD, 
                    " memory_to_reg = ", memory_to_reg, 
                    " aluop = ", aluop, 
                    " alu_src_a = ", alu_src_a, 
                    " alu_src_b = ", alu_src_b,
                    "\n Esperado:\n",
                    "opcode = ", instruction_opcode,
                    " pc_write = ", expected_pc_write,
                    " ir_write = ", expected_ir_write,
                    " pc_source = ", expected_pc_source,
                    " reg_write = ", expected_reg_write,
                    " memory_read = ", expected_memory_read,
                    " is_immediate = ", expected_is_immediate,
                    " memory_write = ", expected_memory_write,
                    " pc_write_cond = ", expected_pc_write_cond,
                    " lorD = ", expected_lorD,
                    " memory_to_reg = ", expected_memory_to_reg,
                    " aluop = ", expected_aluop,
                    " alu_src_a = ", expected_alu_src_a,
                    " alu_src_b = ", expected_alu_src_b);
            end
        #2;

    end
    
    $finish;


end

endmodule