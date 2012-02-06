library ieee, unisim;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use unisim.vcomponents.all;


entity erase is
  port(
    master_clk  : in  std_logic;        -- master clock from oscillator
    clka        : out std_logic;        -- clock generated from master clock and sent to FPGA
    pp_d        : in  std_logic_vector(7 downto 0);  -- parallel port data
    pp_s        : out std_logic_vector(5 downto 3);  -- parallel port status
    fpga_tck    : out std_logic;        -- driver to FPGA JTAG clock
    fpga_cclk   : out std_logic;        -- driver to FPGA config clock
    fpga_prog_n : out std_logic;        -- driver to FPGA program pin
    fpga_cs_n   : out std_logic;        -- driver to FPGA config. chip-select
    fpga_wr_n   : out std_logic;        -- driver to FPGA config. write strobe
    fpga_done   : in  std_logic;        -- input from FPGA done pin
    fpga_d      : out std_logic_vector(7 downto 0);  -- drivers to FPGA data pins and 7-seg LED
    fpga_m      : out std_logic_vector(0 downto 0);  -- FPGA configuration mode control
    fpga_pp_d   : out std_logic_vector(7 downto 0);  -- parallel port data to FPGA
    fpga_pp_s   : in  std_logic_vector(5 downto 3);  -- parallel port status from FPGA
    flash_ce_n  : in  std_logic         -- Flash chip-enable
    );
end entity erase;


architecture arch of erase is

  constant LO                  : std_logic                    := '0';
  constant HI                  : std_logic                    := '1';
  constant SLAVE_PARALLEL_MODE : std_logic_vector(0 downto 0) := "0";
  signal   cclk                : std_logic;
  signal   config_data         : std_logic_vector(3 downto 0);
  signal   clk_divider         : std_logic_vector(1 downto 0);

begin

  fpga_tck <= LO;                       -- deactivate FPGA JTAG circuit
  fpga_m   <= SLAVE_PARALLEL_MODE;      -- configure FPGA with byte-wide data

  -- connect FPGA configuration pins
  u0 : pullup port map(O => fpga_prog_n);  -- place a pullup on PROG#
  u1 : pullup port map(O => fpga_cs_n);  -- place a pullup on CS#
  u2 : pullup port map(O => fpga_wr_n);  -- place a pullup on WR#
  fpga_prog_n <= LO when pp_d(7) = LO   else 'Z';  -- programming pulse comes from parallel port
  fpga_cs_n   <= LO when fpga_done = LO else 'Z';  -- enable writing of bitstream data
  fpga_wr_n   <= LO when fpga_done = LO else 'Z';  --   during the FPGA config. phase

  cclk      <= not pp_d(0);
  fpga_cclk <= not cclk;
  fpga_d    <= (config_data & pp_d(5 downto 2)) when fpga_done = LO  else
               "ZZZZZZ1Z"                       when flash_ce_n = HI else  -- show FPGA config status on LED-DP
               "ZZZZZZZZ";              -- release control of FPGA Flash/LED pins when FPGA lowers Flash chip-enable

  -- store the upper nybble of each bitstream byte
  process(cclk)
  begin
    if(rising_edge(cclk)) then
      config_data <= pp_d(5 downto 2);
    end if;
  end process;

  -- connect the parallel port data and status pins to the FPGA
  fpga_pp_d <= (pp_d(7 downto 2) & not(pp_d(1 downto 0))) when flash_ce_n = HI else "ZZZZZZZZ";
  -- latch and hold the status pin values from the FPGA when the FPGA switches to access the Flash 
  process(fpga_pp_s, flash_ce_n)
  begin
    if(flash_ce_n = HI) then
      pp_s  <= fpga_pp_s;
    end if;
  end process;

  -- generate clocks for the FPGA
  process(master_clk)
  begin
    if(rising_edge(master_clk)) then
      clk_divider <= clk_divider + 1;
    end if;
  end process;
  clka            <= master_clk;         -- master clock
  -- clkb            <= clk_divider(0);  -- master clock / 2

end architecture arch;

