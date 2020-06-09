module Top(
           input        CLK100MHZ,
           output       VGA_HS,
           output       VGA_VS,
           output [3:0] VGA_R,
           output [3:0] VGA_G,
           output [3:0] VGA_B,
           inout [10:1] JA
           );

   wire                 CLK_25MHZ;
   wire                 CLK_LOCKED;
   wire [7:0]           VGA_RED_FULL;
   wire [7:0]           VGA_GREEN_FULL;
   wire [7:0]           VGA_BLUE_FULL;

   assign VGA_R = VGA_RED_FULL[7:4];
   assign VGA_G = VGA_GREEN_FULL[7:4];
   assign VGA_B = VGA_BLUE_FULL[7:4];

   assign AN[7:4] = 4'b1111;

   ClockWiz25 u_ClockWiz25
     (.CLKIN_100MHZ(CLK100MHZ),
      .CLKOUT_25MHZ(CLK_25MHZ),
      .LOCKED(CLK_LOCKED)
      );

   topEntity u_topEntity
     (.CLK_25MHZ(CLK_25MHZ),
      .RESET(!CLK_LOCKED),
      .VGA_HSYNC(VGA_HS),
      .VGA_VSYNC(VGA_VS),
      .VGA_RED(VGA_RED_FULL),
      .VGA_GREEN(VGA_GREEN_FULL),
      .VGA_BLUE(VGA_BLUE_FULL)
      .ROWS(JA[10:7]),
      .COLS(JA[4:1])
      );
endmodule
