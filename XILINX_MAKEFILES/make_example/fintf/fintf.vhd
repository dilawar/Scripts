--
-- CPLD design which controls the loading of the XSA Flash
-- with data from the PC parallel port.
--

library ieee, unisim;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use unisim.vcomponents.all;

entity fintf is
  generic
    (
      ADDR_LEN :     positive := 18     -- number of address bits for XSA FLASH
      );
  port
    (
      -- parallel port data and status pins
      pp_d     : in  std_logic_vector(5 downto 0);  -- data nybble, clk, reset from par. port
      pp_s     : out std_logic_vector(5 downto 3);  -- status nybble to parallel port

      -- Flash data, address, and control pins
      flash_d       : inout std_logic_vector(7 downto 0);  -- data bus to XSA FLASH
      flash_a       : out   std_logic_vector(ADDR_LEN-1 downto 0);  -- address bus to XSA FLASH
      flash_ce_n    : out   std_logic;  -- chip-enable for XSA FLASH
      flash_oe_n    : out   std_logic;  -- output-enable for XSA FLASH
      flash_we_n    : out   std_logic;  -- write-enable for XSA FLASH
      flash_reset_n : out   std_logic;  -- reset for XSA FLASH

      -- FPGA pins
      fpga_prog_n : out std_logic       -- fpga PROGRAM pin
      );
end entity;


architecture arch of fintf is

  constant LO  : std_logic := '0';
  constant HI  : std_logic := '1';
  constant NO  : std_logic := '0';
  constant YES : std_logic := '1';

  -- states for the state machine that programs the Flash
  type flash_state_type is
    (
      load_a20,                         -- load address nybble A23-A20
      load_a16,                         -- load address nybble A19-A16, read data nybble D7-D5
      load_a12,                         -- load address nybble A12-A15, read data nybble D4-D2
      load_a8,                          -- load address nybble A8-A11,  read data nybble D1-D0
      load_a4,                          -- load address nybble A4-A7
      load_a0,                          -- load address nybble A0-A4
      load_d4,                          -- load data nybble D4-D7
      load_d0                           -- load data nybble D0-D3
      );

  signal flash_state, next_flash_state : flash_state_type;
  signal clk, reset                    : std_logic;
  signal nybble                        : std_logic_vector(3 downto 0);
  signal addr, next_addr               : std_logic_vector(ADDR_LEN-1 downto 0);
  signal addr_reg, next_addr_reg       : std_logic_vector(23 downto 0);
  signal data_reg, next_data_reg       : std_logic_vector(3 downto 0);

begin
  fpga_prog_n   <= LO;                  -- keep fpga in reset state so it doesn't interfere
  flash_reset_n <= HI;                  -- remove Flash reset so the chip is enabled
  reset         <= not pp_d(0);         -- Flash prog. state machine reset from LSB of parallel port data
  clk           <= not pp_d(1);         -- state machine clock from next bit of parallel port data
  nybble        <= pp_d(5 downto 2);    -- Flash data nybble from parallel port data

  -- this process directs the state transitions of the Flash programming
  -- state machine and sets the control outputs for each state
  process(addr, addr_reg, flash_d, data_reg, nybble, pp_d, flash_state)
  begin
    -- the following statements set the default values for the outputs
    flash_oe_n    <= HI;                -- Flash chip data pin drivers disabled
    flash_ce_n    <= HI;                -- Flash chip disabled
    flash_we_n    <= HI;                -- no write operations to Flash chip
    flash_d       <= (others => 'Z');   -- no data driven into the Flash chip
    pp_s          <= "111";             -- illegal state reported on status pins
    next_addr     <= addr;              -- Flash address does not change
    next_addr_reg <= addr_reg;
    next_data_reg <= data_reg;          -- Flash data does not change

    -- now use the current state to determine the outputs and the
    -- next state for the Flash programming state machine
    case flash_state is

      when load_a20 =>
        -- load Flash address bits A23-A20 and output the
        -- last complete Flash address that was assembled previously
        flash_d                     <= data_reg & nybble;  -- complete data byte written to Flash
        next_addr_reg(23 downto 20) <= nybble;  -- store A23-A20
        next_addr                   <= addr_reg(ADDR_LEN-1 downto 0);  -- output last addr
        pp_s                        <= "000";  -- report current state through parallel port
        next_flash_state            <= load_a16;  -- go to next state

      when load_a16 =>
        -- load Flash address bits A19-A16, read the contents
        -- from the previous Flash address, and send the upper
        -- 3 bits of the Flash data back through the parallel port
        next_addr_reg(19 downto 16) <= nybble;  -- store A19-A16
        flash_ce_n                  <= LO;  -- enable Flash
        flash_oe_n                  <= LO;  -- read Flash
        pp_s                        <= flash_d(7 downto 5);  -- send upper 3 data bits back to PC
        next_flash_state            <= load_a12;  -- go to next state

      when load_a12 =>
        -- load Flash address bits A15-A12, read the contents
        -- from the previous Flash address, and send the middle
        -- three bits of the Flash data back through the parallel port
        next_addr_reg(15 downto 12) <= nybble;  -- store A15-A12
        flash_ce_n                  <= LO;  -- enable Flash
        flash_oe_n                  <= LO;  -- read Flash
        pp_s                        <= flash_d(4 downto 2);  -- send middle 3 data bits back to PC
        next_flash_state            <= load_a8;  -- go to next state

      when load_a8 =>
        -- load Flash address bits A11-A8
        -- load Flash address bits A11-A8, read the contents
        -- from the previous Flash address, and send the lowest
        -- two bits of the Flash data back through the parallel port
        next_addr_reg(11 downto 8) <= nybble;  -- store A11-A8
        flash_ce_n                 <= LO;  -- enable Flash
        flash_oe_n                 <= LO;  -- read Flash
        pp_s                       <= "0" & flash_d(1 downto 0);  -- send lowest 2 data bits back to PC
        next_flash_state           <= load_a4;  -- go to next state

      when load_a4 =>
        -- load Flash address bits A7-A4
        next_addr_reg(7 downto 4) <= nybble;  -- store A7-A4
        pp_s                      <= "001";  -- report current state through parallel port
        next_flash_state          <= load_a0;  -- go to next state

      when load_a0 =>
        -- load Flash address bits A3-A0
        next_addr_reg(3 downto 0) <= nybble;  -- store A3-A0
        pp_s                      <= "010";  -- report current state through parallel port
        next_flash_state          <= load_d4;  -- go to next state

      when load_d4 =>
        -- output the assembled address to the Flash and load the
        -- upper nybble of data that will be written to the Flash
        next_addr        <= addr_reg(ADDR_LEN-1 downto 0);  -- output complete addr
        flash_ce_n       <= LO;         -- enable the Flash
        next_data_reg    <= nybble;     -- store upper data nybble from par port
        flash_d          <= data_reg & nybble;  -- output data to the Flash
        pp_s             <= "011";      -- report current state through parallel port
        next_flash_state <= load_d0;    -- go to the next state

      when load_d0 =>
        -- now get the lower nybble of data from the parallel port
        -- and write the complete byte to the Flash during the
        -- second half of the clock phase
        flash_ce_n       <= LO;         -- keep the Flash enabled
        flash_we_n       <= clk;        -- write goes low during second half of clock cycle
        flash_d          <= data_reg & nybble;  -- complete data byte written to Flash
        pp_s             <= "100";      -- report current state through parallel port
        next_flash_state <= load_a20;   -- go back to the start

      when others =>
        -- return the state machine to the initial state if it
        -- ever gets into an erroneous state
        next_flash_state <= load_a20;

    end case;
  end process;

  -- update the programming machine state and other registers
  process(reset, clk)
  begin
    if (reset = HI) then
      -- asynchronous reset sets state machine to initial state
      -- and clears data register
      flash_state <= load_a20;
      data_reg    <= (others => '0');

    elsif (clk'event and clk = HI) then
      -- update the machine state and other registers on rising clock edge
      flash_state <= next_flash_state;
      addr_reg    <= next_addr_reg;
      data_reg    <= next_data_reg;
    end if;
  end process;

  -- output Flash addresses one-half cycle early.  This gives the Flash
  -- address time to settle and activate the appropriate location for writing.
  process(clk)
  begin
    -- change Flash address during the second half of the clock cycle
    -- before the machine changes states
    if (clk'event and clk = LO) then
      addr <= next_addr;
    end if;
  end process;

  flash_a <= addr;                      -- output address to the Flash chip

end arch;

