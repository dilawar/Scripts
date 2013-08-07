library ieee, unisim;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
use unisim.vcomponents.all;


entity p3jtag is
  port(
    -- parallel port data and status pins
    pp_d : in  std_logic_vector(6 downto 0);
    pp_s : out std_logic_vector(5 downto 3);

    -- programmable oscillator
    master_clk : in  std_logic;
    clka       : out std_logic;

    -- FPGA configuration pins
    fpga_tck    : out std_logic;        -- driver to FPGA JTAG clock
    fpga_tms    : out std_logic;        -- driver to FPGA JTAG mode input
    fpga_tdi    : out std_logic;        -- driver to FPGA JTAG serial data input
    fpga_tdo    : in  std_logic;        -- input from FPGA JTAG serial data output
    fpga_cclk   : out std_logic;        -- driver to FPGA config clock
    fpga_m      : out std_logic_vector(0 downto 0);  -- FPGA configuration mode pin
    fpga_prog_n : out std_logic         -- driver to FPGA /PROGRAM pin
    );
end p3jtag;


architecture arch of p3jtag is

  constant NO                  : std_logic                    := '0';
  constant YES                 : std_logic                    := '1';
  constant LO                  : std_logic                    := '0';
  constant HI                  : std_logic                    := '1';
  constant SLAVE_PARALLEL_MODE : std_logic_vector(0 downto 0) := "0";
  signal   clk_divider         : std_logic_vector(1 downto 0);

begin

  fpga_prog_n <= 'Z';                   -- external pullup keeps the PROGRAM# pin from going low and erasing the FPGA
  fpga_cclk   <= HI;                    -- keep the configuration clock from pulsing
  fpga_m      <= SLAVE_PARALLEL_MODE;   -- keep FPGA in slave-parallel mode (not really necessary)

  -- the XSA power status is sent back through the parallel port status pin 3
  pp_s(3) <= HI;                        -- tell the PC that the VCC for the XSA board is OK
  -- the cable is detected by sending data through data pin 6 and returning
  -- it on status pins 5 and 7.  Status pin 7 is used by the JTAG TDO
  -- pin of the XC9500 CPLD on the XSA Board, so place a shunt at position "xi"
  -- of jumper J9 to make this connection.
  pp_s(5) <= pp_d(6);

  -- drive the FPGA JTAG pins from the parallel port when tristate
  -- control pin (parallel port data pin 3) is low.
  fpga_tms <= pp_d(2) when pp_d(3) = LO else 'Z';
  fpga_tck <= pp_d(1) when pp_d(3) = LO else 'Z';
  fpga_tdi <= pp_d(0) when pp_d(3) = LO else 'Z';

  -- the JTAG TDO output is sent back through the status pin
  pp_s(4) <= fpga_tdo when pp_d(4) = HI else LO;

  -- generate clocks for the FPGA
  process(master_clk)
  begin
    if(rising_edge(master_clk)) then
      clk_divider <= clk_divider + 1;
    end if;
  end process;
  clka            <= master_clk;        -- master clock
--  clkb            <= clk_divider(0);  -- master clock / 2

end arch;

