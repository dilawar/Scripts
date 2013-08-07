library IEEE, UNISIM;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use UNISIM.VComponents.all;
use WORK.common.all;
use WORK.sdram.all;


package XSASDRAM is

  component XSASDRAMCntl
    generic(
      FREQ         :     natural := 50_000;  -- operating frequency in KHz
      PIPE_EN      :     boolean := false;  -- enable pipelined read operations
      MAX_NOP      :     natural := 10000;  -- number of NOPs before entering self-refresh
      DATA_WIDTH   :     natural := 16;  -- host & SDRAM data width
      NROWS        :     natural := 4096;  -- number of rows in SDRAM array
      NCOLS        :     natural := 512;  -- number of columns in SDRAM array
      HADDR_WIDTH  :     natural := 23;  -- host-side address width
      SADDR_WIDTH  :     natural := 12  -- SDRAM-side address width
      );
    port(
      -- host side
      clk          : in  std_logic;     -- master clock
      bufclk       : out std_logic;     -- buffered master clock
      clk1x        : out std_logic;     -- host clock sync'ed to master clock
      clk2x        : out std_logic;     -- double-speed host clock
      lock         : out std_logic;     -- true when host clock is locked to master clock
      rst          : in  std_logic;     -- reset
      rd           : in  std_logic;     -- initiate read operation
      wr           : in  std_logic;     -- initiate write operation
      earlyOpBegun : out std_logic;     -- read/write/self-refresh op begun     (async)
      opBegun      : out std_logic;     -- read/write/self-refresh op begun (clocked)
      rdPending    : out std_logic;     -- read operation(s) are still in the pipeline
      done         : out std_logic;     -- read or write operation is done
      rdDone       : out std_logic;     -- read done and data is available
      hAddr        : in  std_logic_vector(HADDR_WIDTH-1 downto 0);  -- address from host
      hDIn         : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- data from host
      hDOut        : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- data to host
      status       : out std_logic_vector(3 downto 0);  -- diagnostic status of the FSM         

      -- SDRAM side
      sclkfb : in    std_logic;         -- clock from SDRAM after PCB delays
      sclk   : out   std_logic;         -- SDRAM clock sync'ed to master clock
      cke    : out   std_logic;         -- clock-enable to SDRAM
      cs_n   : out   std_logic;         -- chip-select to SDRAM
      ras_n  : out   std_logic;         -- SDRAM row address strobe
      cas_n  : out   std_logic;         -- SDRAM column address strobe
      we_n   : out   std_logic;         -- SDRAM write enable
      ba     : out   std_logic_vector(1 downto 0);  -- SDRAM bank address bits
      sAddr  : out   std_logic_vector(SADDR_WIDTH-1 downto 0);  -- SDRAM row/column address
      sData  : inout std_logic_vector(DATA_WIDTH-1 downto 0);  -- SDRAM in/out databus
      dqmh   : out   std_logic;         -- high databits I/O mask
      dqml   : out   std_logic          -- low databits I/O mask
      );
  end component;

end package XSASDRAM;



library IEEE, UNISIM;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;
use UNISIM.VComponents.all;
use WORK.common.all;
use WORK.sdram.all;

entity XSASDRAMCntl is
  generic(
    FREQ         :     natural := 50_000;  -- operating frequency in KHz
    PIPE_EN      :     boolean := false;  -- enable pipelined read operations
    MAX_NOP      :     natural := 10000;  -- number of NOPs before entering self-refresh
    DATA_WIDTH   :     natural := 16;   -- host & SDRAM data width
    NROWS        :     natural := 4096;  -- number of rows in SDRAM array
    NCOLS        :     natural := 512;  -- number of columns in SDRAM array
    HADDR_WIDTH  :     natural := 23;   -- host-side address width
    SADDR_WIDTH  :     natural := 12    -- SDRAM-side address width
    );
  port(
    -- host side
    clk          : in  std_logic;       -- master clock
    bufclk       : out std_logic;       -- buffered master clock
    clk1x        : out std_logic;       -- host clock sync'ed to master clock
    clk2x        : out std_logic;       -- double-speed host clock
    lock         : out std_logic;       -- true when host clock is locked to master clock
    rst          : in  std_logic;       -- reset
    rd           : in  std_logic;       -- initiate read operation
    wr           : in  std_logic;       -- initiate write operation
    earlyOpBegun : out std_logic;       -- read/write/self-refresh op begun     (async)
    opBegun      : out std_logic;       -- read/write/self-refresh op begun (clocked)
    rdPending    : out std_logic;       -- read operation(s) are still in the pipeline
    done         : out std_logic;       -- read or write operation is done
    rdDone       : out std_logic;       -- read done and data is available
    hAddr        : in  std_logic_vector(HADDR_WIDTH-1 downto 0);  -- address from host
    hDIn         : in  std_logic_vector(DATA_WIDTH-1 downto 0);  -- data from host
    hDOut        : out std_logic_vector(DATA_WIDTH-1 downto 0);  -- data to host
    status       : out std_logic_vector(3 downto 0);  -- diagnostic status of the FSM         

    -- SDRAM side
    sclkfb : in    std_logic;           -- clock from SDRAM after PCB delays
    sclk   : out   std_logic;           -- SDRAM clock sync'ed to master clock
    cke    : out   std_logic;           -- clock-enable to SDRAM
    cs_n   : out   std_logic;           -- chip-select to SDRAM
    ras_n  : out   std_logic;           -- SDRAM row address strobe
    cas_n  : out   std_logic;           -- SDRAM column address strobe
    we_n   : out   std_logic;           -- SDRAM write enable
    ba     : out   std_logic_vector(1 downto 0);  -- SDRAM bank address bits
    sAddr  : out   std_logic_vector(SADDR_WIDTH-1 downto 0);  -- SDRAM row/column address
    sData  : inout std_logic_vector(DATA_WIDTH-1 downto 0);  -- SDRAM in/out databus
    dqmh   : out   std_logic;           -- high databits I/O mask
    dqml   : out   std_logic            -- low databits I/O mask
    );
end XSASDRAMCntl;



architecture arch of XSASDRAMCntl is

  -- the SDRAM controller and external SDRAM chip will clock on the same edge
  -- if the frequency is greater than the minimum DLL lock frequency
  constant MIN_LOCK_FREQ : natural := 25_000;
  constant IN_PHASE      : boolean := (FREQ >= MIN_LOCK_FREQ);

  -- signals for internal logic clock DLL
  signal int_clkin, int_clk1x, int_clk1x_b,
    int_clk2x, int_clk2x_b, int_lock              : std_logic;
  -- signals for external logic clock DLL
  signal ext_clkin, sclkfb_b, ext_clk1x, ext_lock : std_logic;
  signal clk_i                                    : std_logic;  -- clock for SDRAM controller logic

  signal lock_i : std_logic;

  -- bus for holding output data from SDRAM
  signal sDOut   : std_logic_vector(sData'range);
  signal sDOutEn : std_logic;

begin

  -----------------------------------------------------------
  -- setup the DLLs for clock generation 
  -----------------------------------------------------------

  -- master clock must come from a dedicated clock pin
  clkin : IBUFG port map (I => clk, O => int_clkin);

  -- clock SDRAM and controller on opposite edges if not in-phase
  ext_clkin <= int_clkin when IN_PHASE else not int_clkin;

  -- generate the DLLs for sync'ing the clocks as long as the master clock
  -- has a frequency high enough for the DLLs to lock
  gen_dlls : if IN_PHASE generate

    -- generate an internal clock sync'ed to the master clock
    dllint : CLKDLL port map(
      CLKIN  => int_clkin,
      CLKFB  => int_clk1x_b,
      CLK0   => int_clk1x,
      RST    => ZERO,
      CLK90  => open,
      CLK180 => open,
      CLK270 => open,
      CLK2X  => int_clk2x,
      CLKDV  => open,
      LOCKED => int_lock
      );

    -- sync'ed single and double-speed clocks for use by internal logic
    int_clk1x_buf : BUFG port map(I => int_clk1x, O => int_clk1x_b);
    int_clk2x_buf : BUFG port map(I => int_clk2x, O => int_clk2x_b);

    -- generate an external SDRAM clock sync'ed to the master clock
    sclkfb_buf : IBUFG port map(I => sclkfb, O => sclkfb_b);  -- SDRAM clock with PCB delays
    dllext     : CLKDLL port map(
      CLKIN                       => ext_clkin,
      CLKFB                       => sclkfb_b,
      CLK0                        => ext_clk1x,
      RST                         => ZERO,
      CLK90                       => open,
      CLK180                      => open,
      CLK270                      => open,
      CLK2X                       => open,
      CLKDV                       => open,
      LOCKED                      => ext_lock
      );

  end generate;

  bufclk <= int_clkin;
  clk_i  <= int_clk1x_b when IN_PHASE else int_clkin;
  clk1x  <= int_clk1x_b when IN_PHASE else int_clkin;
  clk2x  <= int_clk2x_b when IN_PHASE else int_clkin;
  sclk   <= ext_clk1x   when IN_PHASE else ext_clkin;

  -- indicate the lock status of the internal and external DLL
  lock_i <= int_lock and ext_lock when IN_PHASE else YES;
  lock   <= lock_i;                     -- lock signal for the host logic

  -- SDRAM memory controller module
  u1 : sdramCntl
    generic map(
      FREQ         => FREQ,
      IN_PHASE     => IN_PHASE,
      PIPE_EN      => PIPE_EN,
      MAX_NOP      => MAX_NOP,
      DATA_WIDTH   => DATA_WIDTH,
      NROWS        => NROWS,
      NCOLS        => NCOLS,
      HADDR_WIDTH  => HADDR_WIDTH,
      SADDR_WIDTH  => SADDR_WIDTH
      )
    port map(
      clk          => clk_i,            -- master clock from external clock source (unbuffered)
      lock         => lock_i,           -- valid synchronized clocks indicator
      rst          => rst,              -- reset
      rd           => rd,               -- host-side SDRAM read control from memory tester
      wr           => wr,               -- host-side SDRAM write control from memory tester
      rdPending    => rdPending,
      opBegun      => opBegun,          -- SDRAM memory read/write done indicator
      earlyOpBegun => earlyOpBegun,     -- SDRAM memory read/write done indicator
      rdDone       => rdDone,           -- SDRAM memory read/write done indicator
      done         => done,
      hAddr        => hAddr,            -- host-side address from memory tester
      hDIn         => hDIn,             -- test data pattern from memory tester
      hDOut        => hDOut,            -- SDRAM data output to memory tester
      status       => status,           -- SDRAM controller state (for diagnostics)
      cke          => cke,              -- SDRAM clock enable
      ce_n         => cs_n,             -- SDRAM chip-select
      ras_n        => ras_n,            -- SDRAM RAS
      cas_n        => cas_n,            -- SDRAM CAS
      we_n         => we_n,             -- SDRAM write-enable
      ba           => ba,               -- SDRAM bank address
      sAddr        => sAddr,            -- SDRAM address
      sDIn         => sData,            -- input data from SDRAM
      sDOut        => sDOut,            -- output data to SDRAM
      sDOutEn      => sDOutEn,          -- enable drivers to send data to SDRAM
      dqmh         => dqmh,             -- SDRAM DQMH
      dqml         => dqml              -- SDRAM DQML
      );

  sData <= sDOut when sDOutEn = YES else (others => 'Z');

end arch;
