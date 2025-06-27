module Control_Unit (
    input wire clk,
    input wire rst_n,
    input wire [6:0] instruction_opcode,
    output reg pc_write,
    output reg ir_write,
    output reg pc_source,
    output reg reg_write,
    output reg memory_read,
    output reg is_immediate,
    output reg memory_write,
    output reg pc_write_cond,
    output reg lorD,
    output reg memory_to_reg,
    output reg [1:0] aluop,
    output reg [1:0] alu_src_a,
    output reg [1:0] alu_src_b
);

// machine states
localparam FETCH              = 4'b0000;
localparam DECODE             = 4'b0001;
localparam MEMADR             = 4'b0010;
localparam MEMREAD            = 4'b0011;
localparam MEMWB              = 4'b0100;
localparam MEMWRITE           = 4'b0101;
localparam EXECUTER           = 4'b0110;
localparam ALUWB              = 4'b0111;
localparam EXECUTEI           = 4'b1000;
localparam JAL                = 4'b1001;
localparam BRANCH             = 4'b1010;
localparam JALR               = 4'b1011;
localparam AUIPC              = 4'b1100;
localparam LUI                = 4'b1101;
localparam JALR_PC            = 4'b1110;

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

// insira aqui o seu código

endmodule
