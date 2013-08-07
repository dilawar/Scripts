----------------------------------------------------------------------------------
-- This design reads an image from SDRAM and displays it on a VGA monitor
----------------------------------------------------------------------------------


library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use work.vga_pckg.all;
use work.xsasdram.all;
use work.common.all;


entity test_vga is
  generic(
    SDRAM_NROWS     :       natural := 4096;  -- 4096 for XSA-50, XSA-100; 8192 for XSA-200, XSA-3S1000
    SDRAM_NCOLS     :       natural := 256;  -- 256 for XSA-50; 512 for XSA-100, XSA-200, XSA-3S1000
    DATA_WIDTH      :       natural := 16;  -- SDRAM databus width
    SADDR_WIDTH     :       natural := 13;  -- # of SDRAM address bits
    HADDR_WIDTH     :       natural := 24;  -- host-side address width
    FREQ            :       natural := 50_000;  -- 50 MHz for XSA-50, XSA-100; 100 MHz for XSA-200, XSA-3S1000
    CLK_DIV         :       natural := 1;  -- pixel clock = FREQ / CLK_DIV
    PIXEL_WIDTH     :       natural := 8;  -- width of a pixel in memory
    NUM_RGB_BITS    :       natural := 2;  -- #bits in each R,G,B component of a pixel
    PIXELS_PER_LINE :       natural := 800;  -- width of image in pixels
    LINES_PER_FRAME :       natural := 600;  -- height of image in scanlines
    FIT_TO_SCREEN   :       boolean := true  -- adapt video timing to fit image width x height
    );
  port(
    rst_n           : in    std_logic;  -- reset
    clk             : in    std_logic;  -- master clock (frequency set by FREQ)
    vsync_n         : out   std_logic;  -- VGA vertical sync
    hsync_n         : out   std_logic;  -- VGA horizontal sync
    red             : out   std_logic_vector(NUM_RGB_BITS-1 downto 0);  -- VGA red signals
    green           : out   std_logic_vector(NUM_RGB_BITS-1 downto 0);  -- VGA green signals
    blue            : out   std_logic_vector(NUM_RGB_BITS-1 downto 0);  -- VGA blue signals
    -- SDRAM I/O
    sclkfb          : in    std_logic;  -- clock from SDRAM after PCB delays
    sclk            : out   std_logic;  -- SDRAM clock sync'ed to master clock
    cke             : out   std_logic;  -- clock-enable to SDRAM
    cs_n            : out   std_logic;  -- chip-select to SDRAM
    ras_n           : out   std_logic;  -- SDRAM row address strobe
    cas_n           : out   std_logic;  -- SDRAM column address strobe
    we_n            : out   std_logic;  -- SDRAM write enable
    ba              : out   std_logic_vector(1 downto 0);  -- SDRAM bank address bits
    sAddr           : out   std_logic_vector(SADDR_WIDTH-1 downto 0);  -- SDRAM row/column address
    sData           : inout std_logic_vector(DATA_WIDTH-1 downto 0);  -- SDRAM in/out databus
    dqmh            : out   std_logic;  -- high databits I/O mask
    dqml            : out   std_logic   -- low databits I/O mask
    );
end entity;



architecture arch of test_vga is

  signal clk1x        : std_logic;      -- sync'ed clock from SDRAM controller
  signal rst          : std_logic;      -- reset signal
  signal eof          : std_logic;      -- end-of-frame signal from VGA controller
  signal earlyOpBegun : std_logic;      -- indicates when an SDRAM read operation has begun
  signal rdDone       : std_logic;      -- indicates when data read from the SDRAM is available
  signal full, full_n : std_logic;      -- indicates when the VGA pixel buffer is full
  signal address      : std_logic_vector(HADDR_WIDTH-1 downto 0);  -- SDRAM address counter 
  signal pixel        : std_logic_vector(DATA_WIDTH-1 downto 0);  -- pixel values from SDRAM

begin

  rst <= not rst_n;

  -- update the SDRAM address counter
  process(clk1x)
  begin
    if rising_edge(clk1x) then
      if eof = YES then
        address <= std_logic_vector(TO_UNSIGNED(0, address'length));  -- reset the address at the end of a video frame
      elsif earlyOpBegun = YES then
        address <= address + 1;         -- go to the next address once the read of the current address has begun
      end if;
    end if;
  end process;

  -- XSA SDRAM controller used to get pixel data from the external SDRAM
  u0 : XSASDRAMCntl
    generic map(
      FREQ         => FREQ,
      PIPE_EN      => true,             -- use pipelining for maximum speed
      MAX_NOP      => 1000000,          -- disable self-refresh since it takes too long to re-awaken the SDRAM with video timing
      DATA_WIDTH   => DATA_WIDTH,
      NROWS        => SDRAM_NROWS,
      NCOLS        => SDRAM_NCOLS,
      HADDR_WIDTH  => HADDR_WIDTH,
      SADDR_WIDTH  => SADDR_WIDTH
      )
    port map(
      -- host side
      clk          => clk,              -- master clock
      clk1x        => clk1x,            -- master clock resync'ed to account for delays to external SDRAM
      rst          => rst,
      rd           => full_n,           -- initiate a read when the VGA pixel buffer is not full
      hAddr        => address,          -- the address to read from is stored in the address counter
      earlyOpBegun => earlyOpBegun,     -- indicate when the read operation has actually begun
      rdDone       => rdDone,           -- indicate when the data from the read operation is available
      hDOut        => pixel,            -- this is the pixel data that was read from the SDRAM
      wr           => '0',              -- no SDRAM writing is needed in this application
      hDIn         => std_logic_vector(TO_UNSIGNED(0, DATA_WIDTH)),  -- set the SDRAM write-data bus to zeroes
      -- SDRAM side
      sclkfb       => sclkfb,
      sclk         => sclk,
      cke          => cke,
      cs_n         => cs_n,
      ras_n        => ras_n,
      cas_n        => cas_n,
      we_n         => we_n,
      ba           => ba,
      sAddr        => sAddr,
      sData        => sData,
      dqmh         => dqmh,
      dqml         => dqml
      );

  -- VGA generator
  u1 : vga
    generic map (
      FREQ            => FREQ,
      CLK_DIV         => CLK_DIV,
      PIXEL_WIDTH     => PIXEL_WIDTH,
      PIXELS_PER_LINE => PIXELS_PER_LINE,
      LINES_PER_FRAME => LINES_PER_FRAME,
      NUM_RGB_BITS    => NUM_RGB_BITS,
      FIT_TO_SCREEN   => FIT_TO_SCREEN
      )
    port map (
      rst             => rst,
      clk             => clk1x,         -- use the resync'ed master clock so VGA generator is in sync with SDRAM
      wr              => rdDone,        -- write to pixel buffer when the data read from SDRAM is available
      pixel_data_in   => pixel,  -- pixel data from SDRAM
      full            => full,          -- indicates when the pixel buffer is full
      eof             => eof,           -- indicates when the VGA generator has finished a video frame
      r               => red,           -- RGB components
      g               => green,
      b               => blue,
      hsync_n         => hsync_n,       -- horizontal sync
      vsync_n         => vsync_n,       -- vertical sync
      blank           => open
      );
  full_n <= not full;                   -- negate the full signal for use in controlling the SDRAM read operation

end arch;
