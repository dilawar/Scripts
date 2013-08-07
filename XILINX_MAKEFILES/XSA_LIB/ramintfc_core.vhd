library IEEE, unisim;
use IEEE.std_logic_1164.all;

package ramintfc_pckg is

  component ramintfc_core
    generic(
      FREQ        :       natural := 50_000;  -- clock frequency in KHz
      DATA_WIDTH  :       natural := 16;  -- width of data
      HADDR_WIDTH :       natural := 24;  -- host-side address width
      SADDR_WIDTH :       natural := 13;  -- SDRAM address bus width
      NROWS       :       natural := 4096;  -- number of rows in each SDRAM bank
      NCOLS       :       natural := 256  -- number of words in each row
      );
    port(
      -- I/O
      ppd         : in    std_logic_vector(7 downto 0);  -- parallel port data
      pps         : out   std_logic_vector(6 downto 3);  -- parallel port status
      ce_n        : out   std_logic;    -- Flash RAM chip-enable
      clkin       : in    std_logic;    -- main clock input
      clkfb       : in    std_logic;    -- feedback clock mirrors SDRAM clock
      data        : inout std_logic_vector(DATA_WIDTH-1 downto 0);  -- data bus to SDRAM
      clkout      : out   std_logic;    -- clock to SDRAM
      cke         : out   std_logic;    -- SDRAM clock-enable
      cs_n        : out   std_logic;    -- SDRAM chip-select
      ras_n       : out   std_logic;    -- SDRAM RAS
      cas_n       : out   std_logic;    -- SDRAM CAS
      we_n        : out   std_logic;    -- SDRAM write-enable
      ba          : out   std_logic_vector( 1 downto 0);  -- SDRAM bank-address
      saddr       : out   std_logic_vector(SADDR_WIDTH-1 downto 0);  -- SDRAM address bus
      dqmh        : out   std_logic;    -- SDRAM DQMH
      dqml        : out   std_logic;    -- SDRAM DQML
      s           : out   std_logic_vector(6 downto 0)  -- 7-segment LED
      );
  end component;

end package ramintfc_pckg;



library IEEE, unisim;
use IEEE.std_logic_1164.all;
use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;
use unisim.vcomponents.all;
use WORK.common.all;
use WORK.xsasdram.all;

entity ramintfc_core is
  generic(
    FREQ        :       natural := 50_000;  -- clock frequency in KHz
    DATA_WIDTH  :       natural := 16;  -- width of data
    HADDR_WIDTH :       natural := 24;  -- host-side address width
    SADDR_WIDTH :       natural := 13;  -- SDRAM address bus width
    NROWS       :       natural := 4096;  -- number of rows in each SDRAM bank
    NCOLS       :       natural := 256  -- number of words in each row
    );
  port(
    -- I/O
    ppd         : in    std_logic_vector(7 downto 0);  -- parallel port data
    pps         : out   std_logic_vector(6 downto 3);  -- parallel port status
    ce_n        : out   std_logic;      -- Flash RAM chip-enable
    clkin       : in    std_logic;      -- main clock input
    clkfb       : in    std_logic;      -- feedback clock mirrors SDRAM clock
    data        : inout std_logic_vector(DATA_WIDTH-1 downto 0);  -- data bus to SDRAM
    clkout      : out   std_logic;      -- clock to SDRAM
    cke         : out   std_logic;      -- SDRAM clock-enable
    cs_n        : out   std_logic;      -- SDRAM chip-select
    ras_n       : out   std_logic;      -- SDRAM RAS
    cas_n       : out   std_logic;      -- SDRAM CAS
    we_n        : out   std_logic;      -- SDRAM write-enable
    ba          : out   std_logic_vector( 1 downto 0);  -- SDRAM bank-address
    saddr       : out   std_logic_vector(SADDR_WIDTH-1 downto 0);  -- SDRAM address bus
    dqmh        : out   std_logic;      -- SDRAM DQMH
    dqml        : out   std_logic;      -- SDRAM DQML
    s           : out   std_logic_vector(6 downto 0)  -- 7-segment LED
    );
end entity;

architecture arch of ramintfc_core is

  -- states for FSM that gets address and data from PC parallel port and
  -- then reads/writes the SDRAM
  type download_state_type is
    (
      load_a20,                         -- load address nybble A23-A20
      load_a16,                         -- load address nybble A19-A16
      load_a12,                         -- load address nybble A12-A15
      load_a8,                          -- load address nybble A8-A11
      load_a4,                          -- load address nybble A4-A7
      load_a0,                          -- load address nybble A0-A4
      load_d12,                         -- load data nybble D15-D12
      load_d8,                          -- load data nybble D11-D8
      load_d4,                          -- load data nybble D7-D4
      load_d0                           -- load data nybble D3-D0
      );
  signal download_state, next_download_state : download_state_type;

  -- address output register that is loaded with contents of address register
  signal hAddr, next_hAddr                    : std_logic_vector(HADDR_WIDTH-1 downto 0);
  -- address register that is loaded by nybbles from parallel port
  signal addr_reg, next_addr_reg              : std_logic_vector(23 downto 0);
  -- data register that is loaded by nybbles from parallel port
  signal data_reg, next_data_reg              : std_logic_vector(15 downto 4);
  -- SDRAM data register
  signal data_in_r                            : std_logic_vector(15 downto 0);
  -- read, write and reset signals synchronized to SDRAM clock
  signal wr0, wr1, wr2, wr3, wr4, wr, wr_next : std_logic;
  signal rd0, rd1, rd2, rd3, rd4, rd, rd_next : std_logic;

  signal reset               : std_logic;  -- FSM reset from parallel port
  signal slowclk, bufslowclk : std_logic;  -- slow clock from parallel port
  signal d                   : std_logic_vector(3 downto 0);  -- nybble from parallel port
  signal status              : std_logic_vector(3 downto 0);  -- FSM status

  signal sdram_reset : std_logic;       -- SDRAM controller reset
  signal clk1x       : std_logic;       -- clock from SDRAM controller
  signal lock        : std_logic;       -- SDRAM clock lock indicator
  signal memOpDone   : std_logic;       -- current SDRAM read/write done
  signal hDOut       : std_logic_vector(DATA_WIDTH-1 downto 0);  -- host-side output databus
  signal hDIn        : std_logic_vector(DATA_WIDTH-1 downto 0);  -- host-side input databus

  -- start out with a reset of the SDRAM controller
  attribute INIT                : string;
  attribute INIT of sdram_reset : signal is "1";

begin

  ce_n <= '1';                          -- disable Flash RAM

  -- release the SDRAM controller reset once the SDRAM clock has stabilized
  process(clk1x)
  begin
    if(clk1x'event and clk1x = HI) then
      if lock = HI then
        sdram_reset <= LO;
      end if;
    end if;
  end process;

  -- connect signals from the parallel port to the FSM
  u2 : ibuf port map(I => ppd(1), O => slowclk);  -- slow FSM clock comes through
  u3 : bufg port map(I => slowclk, O => bufslowclk);  -- one pin of the parallel port
  reset           <= ppd(0);            -- FSM reset
  d               <= ppd(5 downto 2);   -- data nybble that carries address/data for SDRAM ops
  pps(6 downto 3) <= status;            -- FSM status info back to PC

  -- FSM that gathers address and data from the PC on write operations to SDRAM and
  -- passes data from SDRAM back to PC on read operations.
  process(hAddr, addr_reg, data_reg, data_in_r, d, download_state, bufslowclk)
  begin
    -- set default signal values to prevent implied latches
    rd0                             <= NO;
    wr0                             <= NO;
    status                          <= "1111";
    s                               <= "0000000";
    hDin                            <= data_reg & d;
    -- reload registers with their current contents
    next_hAddr                      <= hAddr;  -- address output register
    next_addr_reg                   <= addr_reg;  -- address register
    next_data_reg                   <= data_reg;  -- data register
    case download_state is
      -- first state (reset brings us here)
      when load_a20 =>
        -- MSNybble of address register loaded from parallel port
        next_addr_reg(23 downto 20) <= d(3 downto 0);
        -- load address output register with contents of address register.
        -- this outputs the address loaded in the previous six cycles.
        next_hAddr                  <= addr_reg(HADDR_WIDTH-1 downto 0);
        next_download_state         <= load_a16;  -- go to next state
        status                      <= "0000";  -- output state on status pins
        s                           <= "0000001";  -- indicate current state on LEDs
      when load_a16 =>
        -- next nybble of address register loaded from parallel port
        next_addr_reg(19 downto 16) <= d(3 downto 0);
        next_download_state         <= load_a12;  -- go to next state
        rd0                         <= YES;  -- initiate a read operation of the SDRAM
        status                      <= data_in_r(15 downto 12);  -- pass MSNybble of RAM data back through the status pins
        s                           <= "0000010";  -- indicate current state on LEDs
      when load_a12 =>
        -- next nybble of address register loaded from parallel port
        next_addr_reg(15 downto 12) <= d(3 downto 0);
        next_download_state         <= load_a8;  -- go to next state
        status                      <= data_in_r(11 downto 8);  -- pass next nybble of RAM data back through the status pins
        s                           <= "0000100";  -- indicate current state on LEDs
      when load_a8  =>
        -- next nybble of address register loaded from parallel port
        next_addr_reg(11 downto 8)  <= d(3 downto 0);
        next_download_state         <= load_a4;  -- go to next state
        status                      <= data_in_r(7 downto 4);  -- pass next nybble of RAM data back through the status pins
        s                           <= "0001000";  -- indicate current state on LEDs
      when load_a4  =>
        -- next nybble of address register loaded from parallel port
        next_addr_reg(7 downto 4)   <= d(3 downto 0);
        next_download_state         <= load_a0;  -- go to next state
        status                      <= data_in_r(3 downto 0);  -- pass LSNybble of RAM data back through the status pins
        s                           <= "0010000";  -- indicate current state on LEDs
      when load_a0  =>
        -- next nybble of address register loaded from parallel port
        next_addr_reg(3 downto 0)   <= d(3 downto 0);
        next_download_state         <= load_d12;  -- go to next state
        -- if uploading RAM contents, the reset should now be 
        -- asserted so we go back to the first state
        status                      <= "0101";  -- output state on status pins
        s                           <= "0100000";  -- indicate current state on LEDs
      when load_d12 =>
        -- load address output register with contents of address register.
        next_hAddr                  <= addr_reg(HADDR_WIDTH-1 downto 0);
        -- data register loaded with MSNybble of data from parallel port
        next_data_reg(15 downto 12) <= d;
        next_download_state         <= load_d8;  -- go to next state
        status                      <= "0110";  -- output state on status pins
        s                           <= "1000000";  -- indicate current state on LEDs
      when load_d8  =>
        -- data register loaded with MSNybble of data from parallel port
        next_data_reg(11 downto 8)  <= d;
        next_download_state         <= load_d4;  -- go to next state
        status                      <= "0111";  -- output state on status pins
        s                           <= "1000001";  -- indicate current state on LEDs
      when load_d4  =>
        -- data register loaded with MSNybble of data from parallel port
        next_data_reg(7 downto 4)   <= d;
        next_download_state         <= load_d0;  -- go to next state
        status                      <= "1000";  -- output state on status pins
        s                           <= "1000010";  -- indicate current state on LEDs
      when load_d0  =>
        -- initiate a write to the RAM in the second half of the cycle when
        -- the final nybble of data is setup
        wr0                         <= not bufslowclk;
        -- increment the address register for the next write operation.
        -- the new address is not output until the next cycle.
        next_addr_reg               <= addr_reg + 1;
        -- return to the previous state for another RAM write operation.
        -- use the reset input to break out of the four-state cycle.
        next_download_state         <= load_d12;  -- go to next state
        status                      <= "1001";  -- output state on status pins
        s                           <= "1000100";  -- indicate current state on LEDs
      when others   =>
        -- erroneous state so return to reset state
        next_download_state         <= load_a20;
    end case;
  end process;

  process(reset, bufslowclk)
  begin
    if (reset = YES) then
      -- asynchronous reset
      download_state <= load_a20;        -- reset state machine to start
      data_reg       <= (others => LO);  -- clear data register
      -- DO NOT CLEAR THE ADDRESS REGISTER!!  We use the reset signal
      -- during RAM upload operations to reset the state machine, but
      -- we need to retain the RAM address for the read operation.
    elsif (rising_edge(bufslowclk)) then
      -- update registers on the rising clock edge
      download_state <= next_download_state;
      addr_reg       <= next_addr_reg;
      data_reg       <= next_data_reg;
    end if;
  end process;

  process(bufslowclk)
  begin
    -- update the address presented to the RAM on the falling clock
    -- edge.  This keeps the address change away from the rising clock
    -- edge when the read and write signals go active, so there is no chance of
    -- a race condition.
    if (falling_edge(bufslowclk)) then
      hAddr <= next_hAddr;
    end if;
  end process;

  -- sync the read and write controls from the state machine to the SDRAM clock
  process(clk1x)
  begin
    if(rising_edge(clk1x)) then
      rd1         <= rd0;
      rd2         <= rd1;
      rd3         <= rd2;
      rd4         <= rd_next;
      wr1         <= wr0;
      wr2         <= wr1;
      wr3         <= wr2;
      wr4         <= wr_next;
      -- update data input from SDRAM when acknowledgement comes
      if(memOpDone = HI and rd4 = HI) then
        data_in_r <= hDout;
      end if;
    end if;
  end process;

  -- activate SDRAM read or write on the rising edge of the sync'ed
  -- read or write signal and keep the signal active until the RAM acknowledges
  wr_next <= (wr2 and not wr3) or (wr4 and not memOpDone);
  wr      <= wr_next;                   -- remove write as soon as SDRAM acknowledges
  rd_next <= (rd2 and not rd3) or (rd4 and not memOpDone);
  rd      <= rd_next;                   -- remove read as soon as SDRAM acknowledges

  -- SDRAM controller
  u1 : xsaSDRAMCntl
    generic map(
      FREQ         => FREQ,             -- master clock frequency
      PIPE_EN      => false,
      DATA_WIDTH   => hDIn'length,      -- width of the host and SDRAM databus  
      NROWS        => NROWS,            -- number of rows in the SDRAM
      NCOLS        => NCOLS,            -- number of columns in each row
      HADDR_WIDTH  => hAddr'length,     -- host-side address width
      SADDR_WIDTH  => sAddr'length      -- SDRAM-side address width
      )
    port map(
      clk          => clkin,            -- master clock from external clock source (unbuffered)
      bufclk       => open,             -- buffered master clock output
      clk1x        => clk1x,            -- synchronized master clock (accounts for delays to external SDRAM)
      clk2x        => open,             -- synchronized doubled master clock
      lock         => lock,             -- DLL lock indicator
      rst          => sdram_reset,      -- reset
      rd           => rd,               -- host-side SDRAM read control from memory tester
      wr           => wr,               -- host-side SDRAM write control from memory tester
      rdPending    => open,             -- read operation to SDRAM is in progress
      opBegun      => open,             -- indicates memory read/write has begun
      earlyOpBegun => open,             -- early indicator that memory operation has begun
      rdDone       => open,             -- indicates SDRAM memory read operation is done
      done         => memOpDone,        -- indicates SDRAM memory read or write operation is done
      hAddr        => hAddr,            -- host-side address from memory tester to SDRAM
      hDIn         => hDIn,             -- test data pattern from memory tester to SDRAM
      hDOut        => hDOut,            -- SDRAM data output to memory tester
      status       => open,             -- SDRAM controller state (for diagnostics)
      sclkfb       => clkfb,            -- clock feedback with added external PCB delays
      sclk         => clkout,           -- synchronized clock to external SDRAM
      cke          => cke,              -- SDRAM clock enable
      cs_n         => cs_n,             -- SDRAM chip-select
      ras_n        => ras_n,            -- SDRAM RAS
      cas_n        => cas_n,            -- SDRAM CAS
      we_n         => we_n,             -- SDRAM write-enable
      ba           => ba,               -- SDRAM bank address
      sAddr        => sAddr,            -- SDRAM address
      sData        => data,             -- SDRAM databus
      dqmh         => dqmh,             -- SDRAM DQMH
      dqml         => dqml              -- SDRAM DQML
      );

end architecture;
