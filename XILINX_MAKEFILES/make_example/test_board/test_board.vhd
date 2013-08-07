library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use WORK.common.all;
use WORK.mem.all;
use WORK.xsasdram.all;


entity test_board is
  generic(
    FREQ        :       natural := 50_000;  -- frequency of operation in KHz
    PIPE_EN     :       boolean := true;  -- enable fast, pipelined SDRAM operation
    DATA_WIDTH  :       natural := 16;  -- SDRAM data width
    SADDR_WIDTH :       natural := 13;  -- SDRAM row/col address width
    NROWS       :       natural := 4096;  -- number of rows in the SDRAM
    NCOLS       :       natural := 256;  -- number of columns in each SDRAM row
    BEG_ADDR    :       natural := 16#00_0000#;  -- beginning SDRAM address
    END_ADDR    :       natural := 16#3F_FFFF#;  -- ending SDRAM address
    BEG_TEST    :       natural := 16#00_0000#;  -- beginning test range address
    END_TEST    :       natural := 16#3F_FFFF#  -- ending test range address
    );
  port(
    ce_n        : out   std_logic;      -- Flash RAM chip-enable
    sw2         : in    std_logic;      -- active-low pushbutton input
    clk         : in    std_logic;      -- main clock input from external clock source
    sclkfb      : in    std_logic;      -- feedback SDRAM clock with PCB delays
    sclk        : out   std_logic;      -- clock to SDRAM
    cke         : out   std_logic;      -- SDRAM clock-enable
    cs_n        : out   std_logic;      -- SDRAM chip-select
    ras_n       : out   std_logic;      -- SDRAM RAS
    cas_n       : out   std_logic;      -- SDRAM CAS
    we_n        : out   std_logic;      -- SDRAM write-enable
    ba          : out   std_logic_vector( 1 downto 0);  -- SDRAM bank-address
    sAddr       : out   std_logic_vector(SADDR_WIDTH-1 downto 0);  -- SDRAM address bus
    sData       : inout std_logic_vector(DATA_WIDTH-1 downto 0);  -- data bus to/from SDRAM
    dqmh        : out   std_logic;      -- SDRAM DQMH
    dqml        : out   std_logic;      -- SDRAM DQML
    s           : out   std_logic_vector(6 downto 0);  -- 7-segment LED
    pps         : out   std_logic_vector(6 downto 3)  -- outputs to parallel port status bits
    );
end entity;

architecture arch of test_board is
  constant HADDR_WIDTH : natural := log2(END_ADDR-BEG_ADDR+1);
  signal   rst_i       : std_logic;     -- internal reset signal
  signal   clk_i       : std_logic;     -- internal master clock signal
  signal   clk_b       : std_logic;     -- buffered input (non-DLL) clock
  signal   lock        : std_logic;     -- SDRAM clock DLL lock indicator
  signal   begun       : std_logic;     -- SDRAM operation started indicator
  signal   earlyBegun  : std_logic;     -- SDRAM operation started indicator
  signal   done        : std_logic;     -- SDRAM operation complete indicator
  signal   rdDone      : std_logic;     -- SDRAM operation complete indicator
  signal   hAddr       : std_logic_vector(HADDR_WIDTH-1 downto 0);  -- host address bus
  signal   hDIn        : std_logic_vector(DATA_WIDTH-1 downto 0);  -- host-side data to SDRAM
  signal   hDOut       : std_logic_vector(DATA_WIDTH-1 downto 0);  -- host-side data from SDRAM
  signal   rd          : std_logic;     -- host-side read control signal
  signal   wr          : std_logic;     -- host-side write control signal
  signal   dataIn      : std_logic_vector(DATA_WIDTH-1 downto 0);  -- input databus from SDRAM
  signal   dataOut     : std_logic_vector(DATA_WIDTH-1 downto 0);  -- output databus to SDRAM
  signal   divCnt      : std_logic_vector(22 downto 0);  -- clock divider
  signal   progress    : std_logic_vector(1 downto 0);  -- test progress indicator
  signal   err         : std_logic;     -- test error flag
  signal   rdPending   : std_logic;
  signal   syncPushb   : std_logic_vector(1 downto 0);

  attribute INIT          : string;
  attribute INIT of rst_i : signal is "1";
begin

  ce_n <= '1';                          -- disable Flash RAM

  -- internal reset flag is set active by config. bitstream
  -- and then gets reset after clocks start.
  process(clk_b)
  begin
    if(clk_b'event and clk_b = '1') then
      if lock = NO then
        rst_i <= YES;                   -- keep in reset until DLLs start up and lock
      else
        rst_i <= NO;                    -- release reset once DLLs lock
      end if;
    end if;
  end process;

  -- synchronize the pushbutton to the main clock
  process(clk_b)
  begin
    if(clk_b'event and clk_b = '1') then
      syncPushb <= syncPushb(syncPushb'high-1 downto 0) & not sw2;
    end if;
  end process;

  -- generic memory tester module
  gen_fast_memtest : if PIPE_EN generate
    fast_u0        : memTest
      generic map(
        PIPE_EN    => true,
        DATA_WIDTH => hDIn'length,
        ADDR_WIDTH => hAddr'length,
        BEG_TEST   => BEG_TEST,
        END_TEST   => END_TEST
        )
      port map(
        clk        => clk_i,            -- master internal clock
        rst        => rst_i,            -- reset
        doAgain    => syncPushb(syncPushb'high),  -- re-do the memory test
        begun      => earlyBegun,       -- SDRAM controller operation started
        done       => rdDone,           -- SDRAM controller operation complete
        dIn        => hDOut,            -- host-side data from SDRAM goes to memory tester
        rdPending  => rdPending,        -- tell the memory tester if the SDRAM has pending reads
        rd         => rd,               -- host-side SDRAM read control from memory tester
        wr         => wr,               -- host-side SDRAM write control from memory tester
        addr       => hAddr,            -- host-side address from memory tester
        dOut       => hDIn,             -- host-side data to SDRAM from memory tester
        progress   => progress,         -- current phase of memory test
        err        => err               -- memory test error flag
        );
  end generate;
  gen_slow_memtest : if not PIPE_EN generate
    slow_u0        : memTest
      generic map(
        PIPE_EN    => false,
        DATA_WIDTH => hDIn'length,
        ADDR_WIDTH => hAddr'length,
        BEG_TEST   => BEG_TEST,
        END_TEST   => END_TEST
        )
      port map(
        clk        => clk_i,            -- master internal clock
        rst        => rst_i,            -- reset
        doAgain    => syncPushb(syncPushb'high),  -- re-do the memory test
        begun      => begun,            -- SDRAM controller operation started
        done       => done,             -- SDRAM controller operation complete
        dIn        => hDOut,            -- host-side data from SDRAM goes to memory tester
        rdPending  => rdPending,        -- tell the memory tester if the SDRAM has pending reads
        rd         => rd,               -- host-side SDRAM read control from memory tester
        wr         => wr,               -- host-side SDRAM write control from memory tester
        addr       => hAddr,            -- host-side address from memory tester
        dOut       => hDIn,             -- host-side data to SDRAM from memory tester
        progress   => progress,         -- current phase of memory test
        err        => err               -- memory test error flag
        );
  end generate;

  -- SDRAM memory controller module
  u1 : xsaSDRAMCntl
    generic map(
      FREQ         => FREQ,             -- master clock frequency
      PIPE_EN      => PIPE_EN,
      DATA_WIDTH   => hDIn'length,      -- width of the host and SDRAM databus  
      NROWS        => NROWS,            -- number of rows in the SDRAM
      NCOLS        => NCOLS,            -- number of columns in each row
      HADDR_WIDTH  => hAddr'length,     -- host-side address width
      SADDR_WIDTH  => sAddr'length      -- SDRAM-side address width
      )
    port map(
      clk          => clk,              -- master clock from external clock source (unbuffered)
      bufclk       => clk_b,            -- buffered master clock output
      clk1x        => clk_i,            -- synchronized master clock (accounts for delays to external SDRAM)
      clk2x        => open,             -- synchronized doubled master clock
      lock         => lock,             -- DLL lock indicator
      rst          => rst_i,            -- reset
      rd           => rd,               -- host-side SDRAM read control from memory tester
      wr           => wr,               -- host-side SDRAM write control from memory tester
      rdPending    => rdPending,        -- read operation to SDRAM is in progress
      opBegun      => begun,            -- indicates memory read/write has begun
      earlyOpBegun => earlyBegun,       -- early indicator that memory operation has begun
      rdDone       => rdDone,           -- indicates SDRAM memory read operation is done
      done         => done,             -- indicates SDRAM memory read or write operation is done
      hAddr        => hAddr,            -- host-side address from memory tester to SDRAM
      hDIn         => hDIn,             -- test data pattern from memory tester to SDRAM
      hDOut        => hDOut,            -- SDRAM data output to memory tester
      status       => open,             -- SDRAM controller state (for diagnostics)
      sclkfb       => sclkfb,           -- clock feedback with added external PCB delays
      sclk         => sclk,             -- synchronized clock to external SDRAM
      cke          => cke,              -- SDRAM clock enable
      cs_n         => cs_n,             -- SDRAM chip-select
      ras_n        => ras_n,            -- SDRAM RAS
      cas_n        => cas_n,            -- SDRAM CAS
      we_n         => we_n,             -- SDRAM write-enable
      ba           => ba,               -- SDRAM bank address
      sAddr        => sAddr,            -- SDRAM address
      sData        => sData,            -- SDRAM databus
      dqmh         => dqmh,             -- SDRAM DQMH
      dqml         => dqml              -- SDRAM DQML
      );

  -- indicate the phase of the memory test on the LED   
  s <= "0010010" when progress = "00" else  -- "1"
       "1011101" when progress = "01" else  -- "2"
       "1011011" when progress = "10" else  -- "3"
       "1101101" when err = YES       else  -- "E" when error occurs
       "1110111";                           -- "O" when memory tests OK

  -- generate some slow signals from the master clock
  process(clk_b)
  begin
    if(clk_b'event and clk_b = '1') then
      divCnt <= divCnt+1;
    end if;
  end process;

  -- send a heartbeat signal back to the PC so the testing program knows
  -- the status of the memory test:
  --   50% duty cycle -> test in progress
  --   75% duty cycle -> test passed
  --   25% duty cycle -> test failed
  pps(6) <= divCnt(16)               when progress/="11" else  -- test in progress
            divCnt(16) or divCnt(15) when err = NO       else  -- test passed
            divCnt(16) and divCnt(15);  -- test failed                              
end arch;
